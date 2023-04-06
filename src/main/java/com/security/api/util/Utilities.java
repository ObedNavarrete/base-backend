package com.security.api.util;

import com.security.api.configSecurity.User;
import com.security.api.mapper.UserMapper;
import io.micrometer.common.util.StringUtils;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

@Component
public class Utilities {
    @Autowired HttpServletRequest request;

    public String getClassName(Object object) {
        String[] parts = object.getClass().getName().split("\\.");
        return parts[parts.length - 1];
    }

    public String getUUID() {
        return java.util.UUID.randomUUID().toString();
    }

    public GeneralResponse successResponse(String message, Object data) {
        return GeneralResponse.builder()
                .message(message)
                .data(data)
                .status("200")
                .comment("success")
                .build();
    }

    public GeneralResponse errorResponse(String message) {
        return GeneralResponse.builder()
                .message(message)
                .data(null)
                .status("400")
                .comment("error")
                .build();
    }

    public GeneralResponse exceptionResponse(String message, Exception e) {
        String cause = e.getCause().getCause().getMessage();
        cause = cause.substring(cause.indexOf("Detail:"));
        return GeneralResponse.builder()
                .message(message)
                .data(null)
                .status("400")
                .comment(cause)
                .build();
    }

    public User getLoggedUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() instanceof User user) {
            return user;
        }
        return null;
    }

    public Boolean userLogguedHasRole(String role) {
        User user = getLoggedUser();
        if (user != null) {
            return user.getRoles().stream().anyMatch(r -> r.getName().equals(role));
        }
        return false;
    }

    public Boolean userHasRole(User user, String role) {
        if (user != null) {
            return user.getRoles().stream().anyMatch(r -> r.getName().equals(role));
        }
        return false;
    }

    public String getClientIp() {
        String ip = request.getHeader("X-Forwarded-For");
        if (StringUtils.isBlank(ip) || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("Proxy-Client-IP");
        }
        if (StringUtils.isBlank(ip) || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("WL-Proxy-Client-IP");
        }
        if (StringUtils.isBlank(ip) || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getRemoteAddr();
        }

        // devolver solo una ip en caso de que venga una lista
        if (ip != null && ip.contains(",")) {
            ip = ip.split(",")[0];
        }

        if (ip == null) {
            ip = "127.0.0.1";
        }

        return ip;
    }
}
