package com.security.api.serviceImpl;

import com.security.api.auth.base.Role;
import com.security.api.auth.base.RoleRepository;
import com.security.api.auth.base.User;
import com.security.api.auth.base.UserRepository;
import com.security.api.dto.UserDTO;
import com.security.api.dto.UserRoleDTO;
import com.security.api.mapper.UserMapper;
import com.security.api.service.UserService;
import com.security.api.util.GeneralResponse;
import com.security.api.util.Utilities;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
@Slf4j
@RequiredArgsConstructor
public class UserServiceImpl implements UserService {
    private final UserRepository repository;
    private final UserMapper mapper;
    private final RoleRepository roleRepository;
    private final PasswordEncoder passwordEncoder;
    private final Utilities util;

    @Override
    public GeneralResponse findAll(){
        log.info("Getting all users");
        try {
            List<User> users = repository.findAll();
            if (users.isEmpty()) return util.errorResponse("No users found");
            return util.successResponse("Users found successfully", mapper.toUserLimDTO(users));
        }catch (Exception e){
            log.error("Error getting users: {}", e.getMessage());
            return util.exceptionResponse("Error getting users", e);
        }
    }

    @Override
    public GeneralResponse save(UserDTO user, String roleName) {
        log.info("Saving user");
        if (Boolean.TRUE.equals(repository.existsByEmail(user.getEmail()))) {
            return util.errorResponse("The email is already in use, please use another one");
        }
        if (Boolean.TRUE.equals(repository.existsByPhone(user.getPhone()))) {
            return util.errorResponse("The phone is already in use, please use another one");
        }
        roleName = "ROLE_" + roleName.toUpperCase();

        User userToSave = mapper.toEntity(user);
        Optional<Role> role = roleRepository.findByName(roleName);

        if (role.isEmpty()) {
            return util.errorResponse("Role not found");
        }
        Set<Role> roles = new HashSet<>();
        roles.add(role.get());

        userToSave.setRoles(roles);
        userToSave.setCreatedByIp(util.getClientIp());
        userToSave.setPassword(passwordEncoder.encode(user.getPassword()));

        try {
            repository.save(userToSave);
            return util.successResponse("User saved successfully", null);
        }catch (Exception e){
            log.error("Error saving user: {}", e.getMessage());
            return util.exceptionResponse("Error saving user", e);
        }
    }

    @Override
    public GeneralResponse update(UserDTO user, Integer id) {
        log.info("Updating user");
        if (Boolean.TRUE.equals(repository.existsByEmailAndIdNot(user.getEmail(), id))) {
            log.info("The email is already in use");
            return util.errorResponse("The email is already in use");
        }
        if (Boolean.TRUE.equals(repository.existsByPhoneAndIdNot(user.getPhone(), id))) {
            log.info("The phone is already in use");
            return util.errorResponse("The phone is already in use");
        }

        Optional<User> userToUpdate = repository.findById(id);
        if (userToUpdate.isEmpty()) {
            log.info("The user has not been found");
            return util.errorResponse("The user has not been found");
        }

        userToUpdate.get().setEmail(user.getEmail());
        userToUpdate.get().setPhone(user.getPhone());
        userToUpdate.get().setName(user.getName());
        userToUpdate.get().setEnabled(user.getEnabled());
        userToUpdate.get().setUpdatedByIp(util.getClientIp());

        try {
            repository.save(userToUpdate.get());
            log.info("User updated successfully");
            return util.successResponse("User updated successfully", null);
        }catch (Exception e){
            log.error("Error updating user: {}", e.getMessage());
            return util.exceptionResponse("Error updating user", e);
        }
    }

    @Override
    public GeneralResponse delete(Integer id) {
        log.info("Deleting user with id: {}", id);
        Optional<User> userToDelete = repository.findByPasiveIsFalseAndId(id);
        if (userToDelete.isEmpty()) {
            log.info("User with id: {} not found for delete", id);
            return util.errorResponse("User not found");
        }

        try {
            userToDelete.get().setPasive(true);
            userToDelete.get().setUpdatedByIp(util.getClientIp());
            repository.save(userToDelete.get());
            log.info("User deleted successfully");
            return util.successResponse("User deleted successfully", null);
        }catch (Exception e){
            log.error("Error deleting user: {}", e.getMessage());
            return util.exceptionResponse("Error deleting user", e);
        }
    }

    @Override
    public GeneralResponse getByEmail(String email) {
        log.info("Getting user by email: {}", email);
        Optional<User> user = repository.findByPasiveIsFalseAndEmail(email);
        if (user.isEmpty()) {
            log.info("User not found");
            return util.errorResponse("User not found");
        }
        log.info("User with email: {} found successfully", email);
        return util.successResponse("User found successfully", mapper.toUserLimDTO(user.get()));
    }

    @Override
    public GeneralResponse getByPhone(String phone) {
        log.info("Getting user by phone: {}", phone);
        Optional<User> user = repository.findByPasiveIsFalseAndPhone(phone);
        if (user.isEmpty()) {
            log.info("User not found");
            return util.errorResponse("User not found");
        }
        log.info("User with phone: {} found successfully", phone);
        return util.successResponse("User found successfully", mapper.toUserLimDTO(user.get()));
    }

    @Override
    public GeneralResponse getById(Integer id) {
        log.info("Getting user by id: {}", id);
        Optional<User> user = repository.findByPasiveIsFalseAndId(id);
        if (user.isEmpty()) {
            log.info("User not found");
            return util.errorResponse("User not found");
        }
        log.info("User with id: {} found successfully", id);
        return util.successResponse("User found successfully", mapper.toUserLimDTO(user.get()));
    }

    @Override
    public GeneralResponse addOrRemoveRoleToUser(Boolean add, UserRoleDTO form){
        log.info("Adding role to user");

        Optional<User> user = repository.findByPasiveIsFalseAndId(form.getIdUser());
        if (user.isEmpty()) {
            log.info("User not found");
            return util.errorResponse("User not found");
        }

        var roleName = "ROLE_" + form.getRoleName().toUpperCase();
        Optional<Role> role = roleRepository.findByName(roleName);
        if (role.isEmpty()) {
            log.info("Role not found");
            return util.errorResponse("Role not found");
        }

        if (Boolean.TRUE.equals(add)){
            if (user.get().getRoles().contains(role.get())){
                log.info("The user already has the role");
                return util.errorResponse("The user already has the role");
            }

            Set<Role> roles = user.get().getRoles();
            roles.add(role.get());
            user.get().setRoles(roles);
            user.get().setUpdatedByIp(util.getClientIp());
        }

        if (Boolean.FALSE.equals(add)){
            if (!user.get().getRoles().contains(role.get())){
                log.info("The user does not have the role");
                return util.errorResponse("The user does not have the role");
            }

            Set<Role> roles = user.get().getRoles();
            roles.remove(role.get());
            user.get().setRoles(roles);
            user.get().setUpdatedByIp(util.getClientIp());
        }


        try {
            repository.save(user.get());
            log.info("Role added or removed to user successfully");
            String message = Boolean.TRUE.equals(add) ? "Role added to user successfully" : "Role removed to user successfully";
            return util.successResponse(message, null);
        }catch (Exception e){
            log.error("Error adding role to user: {}", e.getMessage());
            return util.exceptionResponse("Error adding role to user", e);
        }
    }
}
