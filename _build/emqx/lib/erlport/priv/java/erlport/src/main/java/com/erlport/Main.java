package com.erlport;

import com.erlport.core.JPort;

/**
 * @author wangwenhai
 * @date 2020/7/11
 */
public class Main {

    // args: --packet=4 --compressed=0 --buffer_size=2048 --use_stdio
    public static void main(String[] args) {
        JPort.start(args);
    }
}
