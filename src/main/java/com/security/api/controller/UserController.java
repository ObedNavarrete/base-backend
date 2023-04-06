package com.security.api.controller;

import com.security.api.dto.UserDTO;
import com.security.api.service.UserService;
import com.security.api.util.GeneralResponse;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/user")
@Slf4j
@RequiredArgsConstructor
public class UserController {
    private final UserService userService;

    @GetMapping
    public ResponseEntity<GeneralResponse> findAll() {
        return ResponseEntity.ok(userService.findAll());
    }

    @PostMapping
    public ResponseEntity<GeneralResponse> save(@RequestBody @Valid UserDTO user) {
        return ResponseEntity.ok(userService.save(user));
    }
}
