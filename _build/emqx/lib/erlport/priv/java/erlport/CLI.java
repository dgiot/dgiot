package erlport;

import java.io.EOFException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import erlport.terms.Atom;
import erlport.terms.Binary;

public class CLI {

    public static void main(final String[] args) {
    	
    		Map<Class<?>, Object> classInstances = new HashMap<>();
    		
        Options opts = new Options(args);

        Port port = new Port(opts);
        while(true) {
            try {
                Request req = port.read();
                //System.err.printf("Received: %s\n", req);
                try {
                    if (req.type == RequestType.CALL) {
                        Class<?> clazz = Class.forName(req.classname.value);
                        Object instance = classInstances.get(clazz);
                        if (instance == null) {
                        		instance = clazz.newInstance();
                        		classInstances.put(clazz, instance);
                        }
                        Class<?>[] classArgs = new Class[req.args.length];
                        for (int i=0; i<classArgs.length; i++) {
                        		classArgs[i] = Object.class;
                        }
                        Method method = clazz.getMethod(req.functname.value, classArgs);
                        Object result = method.invoke(instance, req.args);

                        if (result == null) {
                            result = new Atom("ok");
                        }
                        //System.err.printf("Result: %s\n", result);

                        // back to response
                        port.write(Response.success(req.requestId, result));
                    }
                } catch (Exception e) {
                    Binary errDesc = Utils.stringToBinary(Utils.getStackTrace(e));
                    System.err.println(errDesc);
                    port.write(Response.failure(req.requestId, errDesc));
                }
            } catch (EOFException e) {
                break;
            } catch (Exception e) {
                Binary errDesc = Utils.stringToBinary(Utils.getStackTrace(e));
                System.err.println(errDesc);
                try {
                    port.write(Response.stop(errDesc));
                } catch (Exception e2) {
                    Binary errDesc2 = Utils.stringToBinary(Utils.getStackTrace(e2));
                    System.err.println(errDesc2);
                }
                break;
            }
        }
    }
}
