package com.security.api.controller;

import com.security.api.dto.UserRecords;
import com.security.api.util.GlobalRecords;
import com.security.api.service.UserService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

@RestController
@RequestMapping("/user")
@Slf4j
@RequiredArgsConstructor
public class UserController {
    private final UserService userService;
    @GetMapping
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<GlobalRecords.ApiResponse> findAll() {
        return ResponseEntity.ok(userService.findAll());
    }

    @PostMapping
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<GlobalRecords.ApiResponse> save(@RequestBody @Valid UserRecords.UserObject user, @RequestParam String role) {
        return ResponseEntity.status(201).body(userService.save(user, role));
    }

    @PutMapping("/{id}")
    public ResponseEntity<GlobalRecords.ApiResponse> update(@RequestBody @Valid UserRecords.UserObject user, @PathVariable Integer id) {
        return ResponseEntity.ok(userService.update(user, id));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<GlobalRecords.ApiResponse> delete(@PathVariable Integer id) {
        return ResponseEntity.ok(userService.delete(id));
    }

    @GetMapping("/email")
    public ResponseEntity<GlobalRecords.ApiResponse> getByEmail(@RequestParam String email) {
        return ResponseEntity.ok(userService.getByEmail(email));
    }

    @GetMapping("/phone")
    public ResponseEntity<GlobalRecords.ApiResponse> getByPhone(@RequestParam String phone) {
        return ResponseEntity.ok(userService.getByPhone(phone));
    }

    @GetMapping("/{id}")
    public ResponseEntity<GlobalRecords.ApiResponse> getById(@PathVariable Integer id) {
        return ResponseEntity.ok(userService.getById(id));
    }

    @PostMapping("/role")
    public ResponseEntity<GlobalRecords.ApiResponse> addOrRemoveRoleToUser(
            @RequestParam(required = false, defaultValue = "true") Boolean add,
            @RequestBody @Valid UserRecords.UserRoleObject form) {
        return ResponseEntity.ok(userService.addOrRemoveRoleToUser(add, form));
    }
}
