package com.erlport.proto;

/**
 * @author wangwenhai
 * @date 2020/7/11
 * File description:
 */

public enum RequestType {
    // Call
    CALL("C"),
    // Message
    MESSAGE("M"),
    // Call Result
    RESULT("r"),
    // Error
    ERROR("e");

    private String value;

    RequestType(String x) {
        this.value = x;
    }

    @Override
    public String toString() {
        return value;
    }
}