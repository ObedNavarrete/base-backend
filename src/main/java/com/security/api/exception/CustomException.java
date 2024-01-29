package com.security.api.exception;

import com.security.api.util.GlobalRecords;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = true)
public class CustomException extends RuntimeException {
    private final Integer status;
    private final transient GlobalRecords.ErrorObject errorObject;

    public CustomException(String message, Integer status, GlobalRecords.ErrorObject errorObject) {
        super(message);
        this.status = status;
        this.errorObject = errorObject;
    }
}
