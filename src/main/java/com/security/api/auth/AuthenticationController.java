package com.security.api.auth;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("")
@RequiredArgsConstructor
public class AuthenticationController {
    private final AuthenticationService service;
    @PostMapping("/auth/register")
    public ResponseEntity<AuthenticationResponse> register(
            @RequestBody RegisterRequest request) {
        return ResponseEntity.ok(service.register(request));
    }

    @PostMapping("/auth/login")
    public ResponseEntity<AuthenticationResponse> login(
            @RequestBody LoginRequest token) {
        return ResponseEntity.ok(service.login(token));
    }

    @PostMapping("/auth/refresh")
    public ResponseEntity<AuthenticationResponse> refresh(
            @RequestParam String request) {
        return ResponseEntity.ok(service.refresh(request));
    }
}
