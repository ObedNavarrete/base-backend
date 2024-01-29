package com.security.api.service.impl;

import com.security.api.dto.UserRecords;
import com.security.api.mapper.UserMapper;
import com.security.api.util.GlobalRecords;
import com.security.api.model.entity.Role;
import com.security.api.model.repository.RoleRepository;
import com.security.api.model.entity.User;
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

@Service
@Slf4j
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
            return util.response(true, "Users found successfully", userMapper.toUserLimitedRecords(users));
        } catch (Exception e) {
            log.error("Error getting users: {}", e.getMessage());
            return util.exceptionResponse("Error getting users", e);
        }
    }

    @Override
    public GlobalRecords.ApiResponse save(UserRecords.UserObject user, String roleName) {
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
    public GlobalRecords.ApiResponse update(UserRecords.UserObject user, Integer id) {
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
            return util.response(true, "The user has been found successfully", userMapper.toUserLimitedRecord(user.get()));
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
            return util.response(true, "The user has been found successfully", userMapper.toUserLimitedRecord(user.get()));
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
            return util.response(true, "User found successfully", userMapper.toUserLimitedRecord(user.get()));
        }

        this.util.throwCustomException(
                "By id user not found", 404, RESOURCE_NOT_FOUND, "The user with id: " + id + " has not been found for get, review it"
        );
        return null;
    }

    @Override
    public GlobalRecords.ApiResponse addOrRemoveRoleToUser(Boolean add, UserRecords.UserRoleObject form) {
        log.info("Adding role to user");
        Optional<User> user = repository.findByPassiveIsFalseAndId(form.idUser());
        Optional<Role> role = roleRepository.findByName(this.getRoleName(form.roleName()));

        if (role.isPresent() && user.isPresent()) {
            if (!Objects.equals(this.util.getLoggedUser().getId(), user.get().getId())){
                this.util.verifyIdentity("add or remove role to user");
            }
            var userEntity = user.get();
            var roleEntity = role.get();
            if (Boolean.TRUE.equals(add)) {
                if (userEntity.getRoles().contains(roleEntity)) {
                    log.info("The user {} already has the role {}", userEntity.getId(), roleEntity.getId());
                    this.util.throwCustomException(
                            "The user already has the role", 400, RESOURCE_NOT_FOUND, "The user already has the role"
                    );
                }

                Set<Role> roles = userEntity.getRoles();
                roles.add(roleEntity);
                userEntity.setRoles(roles);
                userEntity.setUpdatedByIp(util.getClientIp());
            }

            if (Boolean.FALSE.equals(add)) {
                if (!userEntity.getRoles().contains(roleEntity)) {
                    log.info("The user {} does not have the role {}", userEntity.getId(), roleEntity.getId());
                    this.util.throwCustomException(
                            "The user does not have the role", 400, RESOURCE_NOT_FOUND, "The user does not have the role"
                    );
                }

                Set<Role> roles = userEntity.getRoles();
                roles.remove(roleEntity);
                userEntity.setRoles(roles);
                userEntity.setUpdatedByIp(util.getClientIp());
            }

            try {
                repository.save(userEntity);
                log.info("Role added or removed to user successfully");
                String message = Boolean.TRUE.equals(add) ? "Role added to user successfully" : "Role removed to user successfully";
                return this.util.response(true, message, null);
            } catch (Exception e) {
                log.error("Error adding role to user: {}", e.getMessage());
                return util.exceptionResponse("Error adding role to user", e);
            }
        }

        log.info("On addOrRemoveRoleToUser method, role not found");
        this.util.throwCustomException(
                "User or role not found", 404, RESOURCE_NOT_FOUND, "The user or role has not been found, review it"
        );
        return null;
    }

    private String getRoleName(@NotBlank String roleName) {
        return "ROLE_" + roleName.toUpperCase();
    }
}
