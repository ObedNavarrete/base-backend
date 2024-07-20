package com.security.api.service.impl;

import com.security.api.dto.UserDto;
import com.security.api.exception.CustomException;
import com.security.api.mapper.UserMapper;
import com.security.api.model.entity.User;
import com.security.api.util.GlobalRecords;
import com.security.api.model.entity.Role;
import com.security.api.model.repository.RoleRepository;
import com.security.api.model.repository.UserRepository;
import com.security.api.service.UserService;
import com.security.api.util.Utilities;
import jakarta.validation.constraints.NotBlank;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import static com.security.api.util.ErrorCodes.RESOURCE_NOT_FOUND;
import static com.security.api.util.ErrorCodes.UNIQUE_CONSTRAINT_VIOLATION;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserServiceImpl implements UserService {
    private final UserRepository repository;
    private final RoleRepository roleRepository;
    private final PasswordEncoder passwordEncoder;
    private final Utilities util;
    private final UserMapper userMapper;

    @Override
    public GlobalRecords.ApiResponse findAll() {
        log.info("Getting all users");
        try {
            List<User> users = repository.findAll();
            if (users.isEmpty()) {
                return util.response(true, "No users found", null);
            }
            return util.response(true, "Users found successfully", userMapper.toUserLimited(users));
        } catch (Exception e) {
            log.error("Error getting users: {}", e.getMessage());
            return util.exceptionResponse("Error getting users", e);
        }
    }

    @Override
    public GlobalRecords.ApiResponse save(UserDto.User user, String roleName) {
        log.info("Saving user");
        if (Boolean.TRUE.equals(repository.existsByEmail(user.email()))) {
            this.util.throwCustomException(
                    "The email is already in use, please use another one", 409, UNIQUE_CONSTRAINT_VIOLATION, "The email is already in use, please use another one"
            );
        }
        if (Boolean.TRUE.equals(repository.existsByPhone(user.phone()))) {
            this.util.throwCustomException(
                    "The phone is already in use, please use another one", 409, UNIQUE_CONSTRAINT_VIOLATION, "The phone is already in use, please use another one"
            );
        }

        roleName = "ROLE_" + roleName.toUpperCase();
        User userToSave = userMapper.toUser(user);
        Optional<Role> role = roleRepository.findByName(roleName);

        if (role.isPresent()) {
            Set<Role> roles = new HashSet<>();
            roles.add(role.get());

            userToSave.setRoles(roles);
            userToSave.setCreatedByIp(util.getClientIp());
            userToSave.setPassword(passwordEncoder.encode(user.password()));

            try {
                repository.save(userToSave);
                log.info("User saved successfully");
                return util.response(true, "User saved successfully", null);
            } catch (Exception e) {
                log.error("Error saving user: {}", e.getMessage());
                return util.exceptionResponse("Error saving user", e);
            }
        }

        log.info("The role {} has not been found", roleName);
        this.util.throwCustomException(
                "Role not found on DB", 404, RESOURCE_NOT_FOUND, "The role with name: " + roleName + " has not been found, please create it first"
        );
        return null;
    }

    @Override
    public GlobalRecords.ApiResponse update(UserDto.User user, Integer id) {
        log.info("user {} is trying to update user with id: {}", this.util.getLoggedUser().getId(), id);
        if (!Objects.equals(this.util.getLoggedUser().getId(), id)){
            this.util.verifyIdentity("update the user");
        }

        if (Boolean.TRUE.equals(repository.existsByEmailAndIdNot(user.email(), id))) {
            log.info("The email {} is already in use", user.email());
            this.util.throwCustomException(
                    "On update method, The email is already in use", 409, UNIQUE_CONSTRAINT_VIOLATION, "The email is already in use"
            );
        }
        if (Boolean.TRUE.equals(repository.existsByPhoneAndIdNot(user.phone(), id))) {
            log.info("The phone {} is already in use", user.phone());
            this.util.throwCustomException(
                    "On update method, The phone is already in use", 409, UNIQUE_CONSTRAINT_VIOLATION, "The phone is already in use"
            );
        }

        Optional<User> userToUpdate = repository.findById(id);
        if (userToUpdate.isPresent()) {
            userToUpdate.get().setEmail(user.email());
            userToUpdate.get().setPhone(user.phone());
            userToUpdate.get().setName(user.name());
            userToUpdate.get().setEnabled(user.enabled());
            userToUpdate.get().setUpdatedByIp(util.getClientIp());

            try {
                repository.save(userToUpdate.get());
                log.info("User updated successfully");
                return util.response(true, "User updated successfully", null);
            } catch (Exception e) {
                log.error("Error updating user: {}", e.getMessage());
                return util.exceptionResponse("Error updating user", e);
            }
        }

        log.info("The user has not been found");
        this.util.throwCustomException(
                "On update method, user has not been found", 404, RESOURCE_NOT_FOUND, "Note: The user with id: " + id + " has not been found, you can not update it"
        );
        return null;
    }

    @Override
    public GlobalRecords.ApiResponse delete(Integer id) {
        log.info("Deleting user with id: {}", id);if (!Objects.equals(this.util.getLoggedUser().getId(), id)){
            this.util.verifyIdentity("delete the user");
        }

        Optional<User> userToDelete = repository.findByPassiveIsFalseAndId(id);
        if (userToDelete.isPresent()) {
            try {
                userToDelete.get().setPassive(true);
                userToDelete.get().setUpdatedByIp(util.getClientIp());
                repository.save(userToDelete.get());
                log.info("User deleted successfully");
                return util.response(true, "User deleted successfully", null);
            } catch (Exception e) {
                log.error("Error deleting user: {}", e.getMessage());
                return util.exceptionResponse("Error deleting user", e);
            }
        }

        log.info("User with id: {} not found for delete", id);
        this.util.throwCustomException(
                "On delete method, User not found", 404, RESOURCE_NOT_FOUND, "Review it, The user with id: " + id + " has not been found for delete"
        );
        return null;
    }

    @Override
    public GlobalRecords.ApiResponse getByEmail(String email) {
        log.info("Getting user by email: {}", email);
        Optional<User> user = repository.findByPassiveIsFalseAndEmail(email);
        if (user.isPresent()) {
            log.info("User with email: {} found successfully", email);
            if (!Objects.equals(this.util.getLoggedUser().getId(), user.get().getId())){
                this.util.verifyIdentity("get by email");
            }
            return util.response(true, "The user has been found successfully", userMapper.toUserLimited(user.get()));
        }

        log.info("On getByEmail method, user not found");
        this.util.throwCustomException(
                "By email user not found", 404, RESOURCE_NOT_FOUND, "The user with email: " + email + " has not been found for get"
        );
        return null;
    }

    @Override
    public GlobalRecords.ApiResponse getByPhone(String phone) {
        log.info("Getting user by phone: {}", phone);

        Optional<User> user = repository.findByPassiveIsFalseAndPhone(phone);

        if (user.isPresent()) {
            log.info("User with phone: {} found successfully", phone);
            if (!Objects.equals(this.util.getLoggedUser().getId(), user.get().getId())){
                this.util.verifyIdentity("get by phone");
            }
            return util.response(true, "The user has been found successfully", userMapper.toUserLimited(user.get()));
        }

        log.info("On getByPhone method, user not found");
        this.util.throwCustomException(
                "By phone user not found", 404, RESOURCE_NOT_FOUND, "The user with phone: " + phone + " has not been found for get, review it"
        );
        return null;
    }

    @Override
    public GlobalRecords.ApiResponse getById(Integer id) {
        log.info("User {} is trying to get user with id: {}", this.util.getLoggedUser().getId(), id);
        if (!Objects.equals(this.util.getLoggedUser().getId(), id)){
            this.util.verifyIdentity("get by id");
        }
        Optional<User> user = repository.findByPassiveIsFalseAndId(id);
        if (user.isPresent()) {
            log.info("User with id: {} found successfully", id);
            return util.response(true, "User found successfully", userMapper.toUserLimited(user.get()));
        }

        this.util.throwCustomException(
                "By id user not found", 404, RESOURCE_NOT_FOUND, "The user with id: " + id + " has not been found for get, review it"
        );
        return null;
    }

    @Override
    public GlobalRecords.ApiResponse addOrRemoveRoleToUser(Boolean add, UserDto.UserRole form) {
        log.info("Adding or removing role to/from user");

        User user = getUser(form.idUser());
        Role role = getRole(form.roleName());

        verifyIdentity(user);

        if (Boolean.TRUE.equals(add)) {
            addRoleToUser(user, role);
        } else if (Boolean.FALSE.equals(add)) {
            removeRoleFromUser(user, role);
        }

        saveUser(user);

        String message = Boolean.TRUE.equals(add) ? "Role added to user successfully" : "Role removed from user successfully";
        return this.util.response(true, message, null);
    }

    private User getUser(Integer userId) {
        return repository.findByPassiveIsFalseAndId(userId)
                .orElseThrow(() -> {
                    log.info("User not found with id: {}", userId);
                    GlobalRecords.ErrorObject errorObject = new GlobalRecords.ErrorObject("User not found with id", "User not found with id");
                    return new CustomException("User not found", 404, errorObject);
                });
    }

    private Role getRole(String roleName) {
        return roleRepository.findByName(this.getRoleName(roleName))
                .orElseThrow(() -> {
                    log.info("Role not found with name: {}", roleName);
                    GlobalRecords.ErrorObject errorObject = new GlobalRecords.ErrorObject("Role not found with name", "Role not found with name");
                    return new CustomException("Role not found", 404, errorObject);
                });
    }

    private void verifyIdentity(User user) {
        if (!Objects.equals(this.util.getLoggedUser().getId(), user.getId())) {
            this.util.verifyIdentity("add or remove role to user");
        }
    }

    private void addRoleToUser(User user, Role role) {
        if (user.getRoles().contains(role)) {
            log.info("The user {} already has the role {}", user.getId(), role.getId());
            this.util.throwCustomException(
                    "The user already has the role", 400, RESOURCE_NOT_FOUND, "The user already has the role"
            );
        }

        user.getRoles().add(role);
        user.setUpdatedByIp(util.getClientIp());
    }

    private void removeRoleFromUser(User user, Role role) {
        if (!user.getRoles().contains(role)) {
            log.info("The user {} does not have the role {}", user.getId(), role.getId());
            this.util.throwCustomException(
                    "The user does not have the role", 400, RESOURCE_NOT_FOUND, "The user does not have the role"
            );
        }

        user.getRoles().remove(role);
        user.setUpdatedByIp(util.getClientIp());
    }

    private void saveUser(User user) {
        try {
            repository.save(user);
            log.info("Role added or removed to user successfully");
        } catch (Exception e) {
            log.error("Error adding or removing role to user: {}", e.getMessage());
            this.util.exceptionResponse("Error adding or removing role to user", e);
        }
    }


    private String getRoleName(@NotBlank String roleName) {
        return "ROLE_" + roleName.toUpperCase();
    }
}
