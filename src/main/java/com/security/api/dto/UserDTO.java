package com.security.api.dto;

import javax.validation.constraints.NotNull;
import lombok.Data;

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
}
