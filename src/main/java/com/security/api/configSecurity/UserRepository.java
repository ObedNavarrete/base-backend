package com.security.api.configSecurity;

import lombok.NonNull;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface UserRepository extends JpaRepository<User, Integer> {
    Optional<User> findByEmail(String email);
    Optional<User> findByPhone(String phone);
    Optional<User> findById(@NonNull Integer id);
    Optional<User> findByEmailOrPhone(String email, String phone);
    Boolean existsByEmail(String email);
    Boolean existsByPhone(String phone);
}
