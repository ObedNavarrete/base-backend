package com.security.api.dto;

import com.security.api.configSecurity.Role;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.Set;

@Data
public class UserDTO {
    private Integer id;
    @NotNull(message = "The name is required")
    private String name;
    @NotNull(message = "The phone is required")
    private String phone;
    @NotNull(message = "The email is required")
    private String email;
    @NotNull(message = "The enabled property is required")
    private Boolean enabled;
    @NotNull(message = "The password is required")
    private String password;
    @NotNull(message = "The roles are required")
    private Set<RoleDTO> roles;
}
