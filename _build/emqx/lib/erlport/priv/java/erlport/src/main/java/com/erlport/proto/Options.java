package com.erlport.proto;

/**
 * @author wangwenhai
 * @date 2020/7/11
 * File description: Options
 */

public class Options {

    int packet = 4;

    int compressed = 0;
    String method = "--use_stdio";
    private int buffer_size = 16 * 1024;

    public Options(final String[] args) {
        for (String s : args) {
            String[] kv = s.split("=");
            if (kv.length < 1) continue;
            switch (kv[0]) {
                case "--packet":
                    packet = Integer.parseInt(kv[1]);
                    break;
                case "--compressed":
                    compressed = Integer.parseInt(kv[1]);
                    break;
                case "--buffer_size":
                    buffer_size = Integer.parseInt(kv[1]);
                    break;
                case "--use_stdio":
                    method = "use_stdio";
                    break;
                case "--no_use_stdio":
                    method = "no_use_stdio";
                    break;
            }
        }
    }

    @Override
    public String toString() {
        return "Options{" +
                "packet=" + packet +
                ", compressed=" + compressed +
                ", buffer_size=" + buffer_size +
                ", method='" + method + '\'' +
                '}';
    }
}