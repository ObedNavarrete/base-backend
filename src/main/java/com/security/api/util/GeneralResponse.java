package com.security.api.util;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class GeneralResponse {
    private String message;
    private Object data;
    private String status;
    private String comment;
}
