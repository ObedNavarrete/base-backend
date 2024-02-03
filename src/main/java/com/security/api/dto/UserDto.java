package com.security.api.dto;

import com.security.api.valid.Phone;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;

public class UserDto {
    public record User(
            Integer id, @NotBlank String name, @Phone String phone, @Email String email,
            Boolean enabled, @NotBlank String password) {
    }

    public record UserLimited(Integer id, String name, String phone, String email, Boolean enabled, String[] rolesName) {
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
            if (obj instanceof UserLimited user) {
                return this.id.equals(user.id);
            }
            return false;
        }

        @Override
        public int hashCode() {
            return this.id.hashCode();
        }
    }

    public record UserRole(Integer idUser, String roleName) {
    }
}
