package com.security.api.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;

import java.util.Set;

@Data
public class UserLimDTO {
    private Integer id;
    private String name;
    private String phone;
    private String email;
    private Boolean enabled;

    @JsonIgnore
    private Set<RoleDTO> roles;

    private String[] rolesName;
    public String[] getRolesName() {
        String[] rolesN = new String[roles.size()];
        int i = 0;
        for (RoleDTO role : roles) {
            rolesN[i] = role.getName();
            i++;
        }
        return rolesN;
    }
}
