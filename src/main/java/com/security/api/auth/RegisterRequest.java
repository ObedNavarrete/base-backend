package com.security.api.auth;

import com.security.api.auth.base.Role;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.HashSet;
import java.util.Set;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RegisterRequest {
    private Integer id;
    private String name;
    private String phone;
    private String email;
    private Boolean enabled;
    private String password;
    private Set<Role> roles = new HashSet<>();
}
