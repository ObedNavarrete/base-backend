package com.security.api.controller;

import com.security.api.dto.UserDTO;
import com.security.api.service.UserService;
import com.security.api.util.GeneralResponse;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/user")
@Slf4j
@RequiredArgsConstructor
public class UserController {
    private final UserService userService;

    @PreAuthorize("hasRole('ROLE_SA_ADMIN')")
    @GetMapping
    public ResponseEntity<GeneralResponse> findAll() {
        return ResponseEntity.ok(userService.findAll());
    }

    @PostMapping
    public ResponseEntity<GeneralResponse> save(@RequestBody @Valid UserDTO user, @RequestParam String role) {
        return ResponseEntity.ok(userService.save(user, role));
    }

    @PutMapping("/{id}")
    public ResponseEntity<GeneralResponse> update(@RequestBody @Valid UserDTO user, @PathVariable Integer id) {
        return ResponseEntity.ok(userService.update(user, id));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<GeneralResponse> delete(@PathVariable Integer id) {
        return ResponseEntity.ok(userService.delete(id));
    }
}
