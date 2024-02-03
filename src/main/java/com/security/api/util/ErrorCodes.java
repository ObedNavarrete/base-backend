package com.security.api.util;

public class ErrorCodes {
    private ErrorCodes() {
        throw new IllegalStateException("Utility class");
    }
    public static final String INVALID_ARGUMENT = "001";
    public static final String DATA_INTEGRITY_VIOLATION = "002";
    public static final String RESOURCE_NOT_FOUND = "003";
    public static final String UNIQUE_CONSTRAINT_VIOLATION = "004";
    public static final String JWT_ERROR = "005";
    public static final String INVALID_CREDENTIALS = "006";
    public static final String ACCESS_DENIED = "007";
    public static final String METHOD_OBJECT_VALIDATION = "008";
}
