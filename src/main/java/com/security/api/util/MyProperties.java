package com.security.api.util;

import lombok.Data;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Data
@Component
public class MyProperties {
    @Value("${jwt.secret}")
    private String jwtSecret;

    @Value("${jwt.expiration.access}")
    private Integer jwtExpirationAccess;

    @Value("${jwt.expiration.refresh}")
    private Integer jwtExpirationRefresh;
}
