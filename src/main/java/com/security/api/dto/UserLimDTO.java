package com.security.api.dto;

import lombok.Data;

import java.util.Set;

@Data
public class UserLimDTO {
    private String name;
    private String phone;
    private String email;
    private Boolean enabled;
    private Set<RoleDTO> roles;
}
