package com.security.api.service.impl;

import com.security.api.model.entity.User;
import com.security.api.model.entity.Role;
import com.security.api.model.repository.RoleRepository;
import com.security.api.model.repository.UserRepository;
import com.security.api.service.AuthService;
import com.security.api.service.JwtService;
import com.security.api.util.GlobalRecords;
import com.security.api.util.Utilities;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static com.security.api.util.ErrorCodes.INVALID_CREDENTIALS;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthServiceImpl implements AuthService {
    private final Utilities util;
    private final UserRepository repository;
    private final PasswordEncoder passwordEncoder;
    private final RoleRepository roleRepository;
    private final JwtService jwtService;
    private final AuthenticationManager authenticationManager;
    private static final String ACCESS_TOKEN = "accessToken";
    private static final String REFRESH_TOKEN = "refreshToken";

    @Override
    public GlobalRecords.ApiResponse register(GlobalRecords.RegisterRequest request) {
        Set<Role> roles = new HashSet<>();
        roles.add(roleRepository.findByName("ROLE_USER").orElseThrow());

        var user = User.builder()
                .name(request.name())
                .phone(request.phone())
                .email(request.email())
                .enabled(true)
                .password(passwordEncoder.encode(request.password()))
                .roles(roles)
                .build();
        user.setCreatedByIp(this.util.getClientIp());
        user = repository.save(user);

        var accessToken = jwtService.generateToken(user);
        var refreshToken = jwtService.generateRefreshToken(user.getId().toString());

        Map<String, Object> response = Map.of(
                ACCESS_TOKEN, accessToken,
                REFRESH_TOKEN, refreshToken
        );

        return this.util.response(true, "User registered", response);
    }

    @Override
    public GlobalRecords.ApiResponse login(GlobalRecords.LoginRequest request) {
        try {
            authenticationManager.authenticate(
                    new UsernamePasswordAuthenticationToken(
                            request.emailOrPhone(),
                            request.password()
                    )
            );
        } catch (Exception e) {
            log.error("Error authenticating {} in method {}, class {}",
                    request.emailOrPhone(), ".login", this.getClass().getName());
            this.util.throwCustomException(
                    "Invalid credentials", 403, INVALID_CREDENTIALS, "The credentials are invalid"
            );
        }

        var us = repository.findByEmailOrPhone(request.emailOrPhone(), request.emailOrPhone());
        if (us.isPresent()) {
            var user = us.get();
            var emailOrPhone = user.getEmail() != null ? user.getEmail() : user.getPhone();
            var uop = emailOrPhone != null ? emailOrPhone : "";

            if (user.getRoles().isEmpty()) {
                log.error("The user {} has no roles, failed in method {}, class {}",
                        uop, "login", this.getClass().getName());
                this.util.throwCustomException(
                        "User has no roles", 403, INVALID_CREDENTIALS, "User has no roles, please contact the administrator"
                );
            }

            Map<String, Object> claims = Map.of(
                    "roles", user.getRoles().stream().map(Role::getName).toArray(),
                    "id", user.getId(),
                    "name", user.getName() != null ? user.getName() : "",
                    "emailOrPhone", uop
            );

            var jwtToken = jwtService.generateToken(claims, user);
            var refreshToken = jwtService.generateRefreshToken(user.getId().toString());

            Map<String, String> response = Map.of(
                    ACCESS_TOKEN, jwtToken,
                    REFRESH_TOKEN, refreshToken
            );

            return this.util.response(true, "User logged in", response);
        }

        log.error("User {} not found, failed in method {}, class {}",
                request.emailOrPhone(), "..login", this.getClass().getName());
        this.util.throwCustomException(
                "User not found on db", 403, INVALID_CREDENTIALS, "The user was not found"
        );
        return null;
    }

    @Override
    public GlobalRecords.ApiResponse refresh(String token) {
        var subject = jwtService.extractUsername(token);
        var user = repository.findById(Integer.parseInt(subject))
                .orElse(null);

        if (Objects.isNull(user)) {
            log.error("User {} not found, failed in method {}, class {}",
                    subject, "refresh", this.getClass().getName());
            this.util.throwCustomException(
                    "User not found", 403, INVALID_CREDENTIALS, "The user was not found"
            );
            return null;
        }

        Map<String, Object> claims = Map.of(
                "roles", user.getRoles().stream().map(Role::getName).toArray(),
                "id", user.getId(),
                "name", user.getName(),
                "phone", user.getPhone(),
                "email", user.getEmail()
        );

        if (Boolean.TRUE.equals(!user.getEnabled() || Objects.isNull(user.getRoles())) || user.getRoles().isEmpty()) {
            log.error("User {} has no roles, failed in method {}, class {}",
                    subject, "refresh", this.getClass().getName());
            this.util.throwCustomException(
                    "User has no roles", 403, INVALID_CREDENTIALS, "User has no roles, or is disabled, please contact the administrator"
            );
            return null;
        }

        var jwtToken = jwtService.generateToken(claims, user);
        var refreshToken = jwtService.generateRefreshToken(user.getId().toString());

        Map<String, String> response = Map.of(
                ACCESS_TOKEN, jwtToken,
                REFRESH_TOKEN, refreshToken
        );

        return this.util.response(true, "Token refreshed", response);
    }
}
