package com.security.api.model.repository;

import com.security.api.model.entity.User;
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
    Boolean existsByEmailAndIdNot(String email, Integer id);
    Boolean existsByPhoneAndIdNot(String phone, Integer id);
    Optional<User> findByPassiveIsFalseAndId(Integer id);
    Optional<User> findByPassiveIsFalseAndEmail(String email);
    Optional<User> findByPassiveIsFalseAndPhone(String phone);
}
