import java.io.*;
import java.util.*;
import com.erlport.*;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

public class Echo {

    public static Object echo(Object r) {
      return r;
    }

    public static Object rev_call(Object pid, Object x) throws Exception{
      try {
            //System.err.println("Echo.java:Pid:" + pid + " X:" + x);
            Object result = Erlang.call("erlport_SUITE", "handle_call", new Object[]{pid, x}, 5000);
            //System.err.println("Echo.java:Result:" + result);
            return result;
      } catch (InterruptedException | ExecutionException | TimeoutException e) {
            e.printStackTrace();
      } 
      return null;
    }
}
