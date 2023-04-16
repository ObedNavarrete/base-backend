package com.security.api.util;

import com.security.api.auth.base.User;
import javax.servlet.http.HttpServletRequest;
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
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("Proxy-Client-IP");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("WL-Proxy-Client-IP");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("HTTP_CLIENT_IP");
        }

        if (ip != null && ip.contains(",")) {
            ip = ip.split(",")[0];
        }

        if (ip == null) {
            ip = "127.0.0.1";
        }

        return ip;
    }
}
