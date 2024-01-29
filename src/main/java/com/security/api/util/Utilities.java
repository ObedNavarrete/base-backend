package com.security.api.util;

import com.security.api.model.entity.User;
import com.security.api.exception.CustomException;
import jakarta.servlet.http.HttpServletRequest;

import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import java.util.Objects;

import static com.security.api.util.ErrorCodes.ACCESS_DENIED;

@Slf4j
@Component
@RequiredArgsConstructor
public class Utilities {
    private final HttpServletRequest request;
    private static final String UNKNOWN = "Unknown";

    public String getClassName(Object object) {
        String[] parts = object.getClass().getName().split("\\.");
        return parts[parts.length - 1];
    }

    public String getUUID() {
        return java.util.UUID.randomUUID().toString();
    }

    public GlobalRecords.ApiResponse response(@NonNull Boolean success, @NonNull String message, Object data) {
        return new GlobalRecords.ApiResponse(success, message, data);
    }

    public GlobalRecords.ApiResponse exceptionResponse(@NotBlank String message, @NotNull Exception e) {
        String cause = e.getCause().getCause().getMessage();
        cause = cause.substring(cause.indexOf("Detail:"));
        return this.response(false, message, cause);
    }

    public User getLoggedUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() instanceof User user) {
            return user;
        }
        return null;
    }

    public Boolean userLoggedHasRole(@NotBlank String role) {
        User user = getLoggedUser();
        if (Objects.nonNull(user)) {
            return user.getRoles().stream().anyMatch(r -> r.getName().equals(role));
        }
        return false;
    }

    public Boolean userHasRole(@NotNull User user, @NotNull String role) {
        return user.getRoles().stream().anyMatch(r -> r.getName().equals(role));
    }

    public Boolean currentUserIsAdmin() {
        User user = getLoggedUser();
        if (Objects.nonNull(user)) {
            return user.getRoles().stream().anyMatch(r -> r.getName().equals("ROLE_ADMIN"));
        }
        return false;
    }

    public Integer currentUserId() {
        return this.getLoggedUser().getId();
    }

    public void verifyIdentity(@NotBlank String action) {
        if (Boolean.FALSE.equals(this.currentUserIsAdmin())) {
            this.throwCustomException(
                    "Fatal Action",
                    HttpServletResponse.SC_UNAUTHORIZED,
                    ACCESS_DENIED,
                    "You don't have permission to " + action
            );
        }
    }

    public String getClientIp() {
        String ip = request.getHeader("X-Forwarded-For");
        if (ip == null || ip.isEmpty() || UNKNOWN.equalsIgnoreCase(ip)) {
            ip = request.getHeader("Proxy-Client-IP");
        }
        if (ip == null || ip.isEmpty() || UNKNOWN.equalsIgnoreCase(ip)) {
            ip = request.getHeader("WL-Proxy-Client-IP");
        }
        if (ip == null || ip.isEmpty() || UNKNOWN.equalsIgnoreCase(ip)) {
            ip = request.getHeader("HTTP_CLIENT_IP");
        }
        if (ip == null || ip.isEmpty() || UNKNOWN.equalsIgnoreCase(ip)) {
            ip = request.getHeader("HTTP_X_FORWARDED_FOR");
        }
        if (ip == null || ip.isEmpty() || UNKNOWN.equalsIgnoreCase(ip)) {
            ip = request.getRemoteAddr();
        }
        if (ip != null && ip.contains(",")) {
            ip = ip.split(",")[0];
        }
        return ip;
    }

    public void throwCustomException(
            @NotBlank String message, @NotBlank Integer httpStatus,
            @NotBlank String errorCode, @NotBlank String errorDetail) {
        throw new CustomException(message,
                httpStatus,
                new GlobalRecords.ErrorObject(errorCode, errorDetail)
        );
    }

    public GlobalRecords.ApiResponse responseForFilter(
            @NotBlank String message, @NotBlank Integer httpStatus,
            @NotBlank String errorCode, @NotBlank String errorDetail) {
        log.info("Status: {}, Message: {}, Error code: {}, Error detail: {}",
                httpStatus, message, errorCode, errorDetail);
        return this.response(false, message,
                new GlobalRecords.ErrorObject(errorCode, errorDetail));
    }
}
