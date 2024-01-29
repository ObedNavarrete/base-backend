package com.security.api.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

public class UserRecords {
    public record UserObject(
            Integer id, @NotBlank String name,
            @NotBlank
            @Pattern(regexp = "^[0-9]*$", message = "The phone must be numeric")
            @Size(min = 8, max = 8, message = "The phone must be 8 digits")
            String phone, @Email String email, Boolean enabled, @NotBlank String password) {
    }

    public record UserLimitedObject(Integer id, String name, String phone, String email, Boolean enabled, String[] rolesName) {
        @Override
        public String toString() {
            return "{" +
                    "id=" + id +
                    ", name='" + name + '\'' +
                    ", phone='" + phone + '\'' +
                    ", email='" + email + '\'' +
                    ", enabled=" + enabled +
                    ", rolesName=" + String.join(", ", rolesName) +
                    '}';
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof UserLimitedObject user) {
                return this.id.equals(user.id);
            }
            return false;
        }

        @Override
        public int hashCode() {
            return this.id.hashCode();
        }
    }

    public record UserRoleObject(Integer idUser, String roleName) {
    }
}
