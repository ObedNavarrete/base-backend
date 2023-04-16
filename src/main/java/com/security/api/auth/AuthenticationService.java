package com.security.api.auth;

import com.security.api.auth.base.User;
import com.security.api.configuration.JwtService;
import com.security.api.auth.base.Role;
import com.security.api.auth.base.RoleRepository;
import com.security.api.auth.base.UserRepository;
import io.jsonwebtoken.ExpiredJwtException;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

@Service
@RequiredArgsConstructor
public class AuthenticationService {
    private final UserRepository repository;
    private final PasswordEncoder passwordEncoder;
    private final RoleRepository roleRepository;
    private final JwtService jwtService;
    private final AuthenticationManager authenticationManager;

    public AuthenticationResponse register(RegisterRequest request) {
        Role r1 = roleRepository.save(
                Role.builder()
                        .name("ROLE_USER")
                        .build());
        Role r2 = roleRepository.save(
                Role.builder()
                        .name("ROLE_ADMIN")
                        .build());
        Set<Role> roles = new HashSet<>();
        roles.add(r1);
        roles.add(r2);

        var user = User.builder()
                .name(request.getName())
                .phone(request.getPhone())
                .email(request.getEmail())
                .enabled(true)
                .password(passwordEncoder.encode(request.getPassword()))
                .roles(roles)
                .build();
        repository.save(user);

        var jwtToken = jwtService.generateToken(user);
        return AuthenticationResponse.builder()
                .accessToken(jwtToken)
                .build();
    }

    public AuthenticationResponse login(LoginRequest request) {
        try {
            authenticationManager.authenticate(
                    new UsernamePasswordAuthenticationToken(
                            request.getEmailOrPhone(),
                            request.getPassword()
                    )
            );
        } catch (Exception e) {
            throw new BadCredentialsException("Invalid username or password");
        }

        var us = repository.findByEmailOrPhone(request.getEmailOrPhone(), request.getEmailOrPhone());

        if (us.isEmpty()) {
            throw new UsernameNotFoundException("User not found");
        }

        var user = us.get();
        var emailOrPhone = user.getEmail() != null ? user.getEmail() : user.getPhone();
        var uop = emailOrPhone != null ? emailOrPhone : "";

        if (user.getRoles().isEmpty()) {
            return AuthenticationResponse.builder()
                    .accessToken(null)
                    .refreshToken(null)
                    .error("User has no roles")
                    .build();
        }

        Map<String, Object> claims = Map.of(
                "roles", user.getRoles().stream().map(Role::getName).toArray(),
                "id", user.getId(),
                "name", user.getName() != null ? user.getName() : "",
                "emailOrPhone", uop
        );

        var jwtToken = jwtService.generateToken(claims, user);
        var refreshToken = jwtService.generateRefreshToken(user.getId().toString());

        return AuthenticationResponse.builder()
                .accessToken(jwtToken)
                .refreshToken(refreshToken)
                .build();
    }

    // Refresh token
    public AuthenticationResponse refresh(String token) {
        try {
            var subject = jwtService.extractUsername(token);

            var user = repository.findById(Integer.parseInt(subject))
                    .orElseThrow(() -> new RuntimeException("User not found"));

            Map<String, Object> claims = Map.of(
                    "roles", user.getRoles().stream().map(Role::getName).toArray(),
                    "id", user.getId(),
                    "name", user.getName() != null ? user.getName() : "Obed",
                    "phone", user.getPhone() != null ? user.getPhone() : "123456789",
                    "email", user.getEmail() != null ? user.getEmail() : "nd@gmail"
            );

            var jwtToken = jwtService.generateToken(claims, user);
            var refreshToken = jwtService.generateRefreshToken(user.getId().toString());

            return AuthenticationResponse.builder()
                    .accessToken(jwtToken)
                    .refreshToken(refreshToken)
                    .build();
        }catch (Exception e) {
            e.printStackTrace();
            throw new BadCredentialsException("The refresh token is invalid");
        }
    }
}
