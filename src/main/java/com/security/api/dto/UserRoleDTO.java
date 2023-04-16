package com.security.api.dto;

import lombok.Data;

import javax.validation.constraints.NotNull;

@Data
public class UserRoleDTO {
    @NotNull(message = "El usario es requerido")
    private Integer idUser;
    @NotNull(message = "El rol es requerido")
    private String roleName;
}
