package com.security.api.util;

import com.security.api.model.entity.Role;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import java.util.HashSet;
import java.util.Set;

public class GlobalRecords {
    public record RegisterRequest(
            @NotBlank @Size(max = 30, message = "The name must be max 30 chars") String name,
            @NotBlank @Size(min = 8, max = 8) String phone,
            @Email String email,
            Boolean enabled,
            @NotBlank String password,
            Set<Role> roles)
    {
        public RegisterRequest {
            roles = roles != null ? roles : new HashSet<>();
            enabled = true;
        }
    }

    public record LoginRequest(
            @NotBlank String emailOrPhone,
            @NotBlank String password) {
    }

    public record ApiResponse(
            Boolean success,
            String message,
            Object data) {
    }

    public record ApiSubError(
            String field,
            String message) {
    }

    public record ErrorObject(
            String errorCode,
            Object errorDetail) {
    }
}
