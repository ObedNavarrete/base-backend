package com.security.api.controller;

import com.security.api.service.AuthService;
import com.security.api.util.GlobalRecords;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
public class AuthController {
    private final AuthService service;
    @PostMapping("/auth/register")
    public ResponseEntity<GlobalRecords.ApiResponse> register(
            @RequestBody @Valid GlobalRecords.RegisterRequest request) {
        return ResponseEntity.ok(service.register(request));
    }

    @PostMapping("/auth/login")
    public ResponseEntity<GlobalRecords.ApiResponse> login(
            @RequestBody @Valid GlobalRecords.LoginRequest request) {
        return ResponseEntity.ok(service.login(request));
    }

    @PostMapping("/auth/refresh")
    public ResponseEntity<GlobalRecords.ApiResponse> refresh(
            @RequestParam String request) {
        return ResponseEntity.ok(service.refresh(request));
    }
}
