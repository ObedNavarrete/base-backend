package com.security.api.serviceImpl;

import com.security.api.auth.base.Role;
import com.security.api.auth.base.RoleRepository;
import com.security.api.auth.base.User;
import com.security.api.auth.base.UserRepository;
import com.security.api.dto.UserDTO;
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
    private final Utilities utilities;

    @Override
    public GeneralResponse findAll(){
        log.info("Getting all users");
        try {
            List<User> users = repository.findAll();
            if (users.isEmpty())
                return utilities.errorResponse("No users found"
                );

            return utilities.successResponse("Users found successfully", mapper.toUserLimDTO(users));
        }catch (Exception e){
            log.error("Error getting users: {}", e.getMessage());
            return utilities.exceptionResponse("Error getting users", e);
        }
    }

    @Override
    public GeneralResponse save(UserDTO user, String roleName) {
        log.info("Saving user");
        if (Boolean.TRUE.equals(repository.existsByEmail(user.getEmail()))) {
            return utilities.errorResponse("The email is already in use, please use another one");
        }
        if (Boolean.TRUE.equals(repository.existsByPhone(user.getPhone()))) {
            return utilities.errorResponse("The phone is already in use, please use another one");
        }
        roleName = "ROLE_" + roleName.toUpperCase();

        User userToSave = mapper.toEntity(user);
        Optional<Role> role = roleRepository.findByName(roleName);

        if (role.isEmpty()) {
            return utilities.errorResponse("Role not found");
        }
        Set<Role> roles = new HashSet<>();
        roles.add(role.get());

        userToSave.setRoles(roles);
        userToSave.setCreatedByIp(utilities.getClientIp());
        userToSave.setPassword(passwordEncoder.encode(user.getPassword()));

        try {
            repository.save(userToSave);
            return utilities.successResponse("User saved successfully", null);
        }catch (Exception e){
            log.error("Error saving user: {}", e.getMessage());
            return utilities.exceptionResponse("Error saving user", e);
        }
    }

    @Override
    public GeneralResponse update(UserDTO user, Integer id) {
        log.info("Updating user");
        if (Boolean.TRUE.equals(repository.existsByEmailAndIdNot(user.getEmail(), id))) {
            log.info("The email is already in use");
            return utilities.errorResponse("The email is already in use");
        }
        if (Boolean.TRUE.equals(repository.existsByPhoneAndIdNot(user.getPhone(), id))) {
            log.info("The phone is already in use");
            return utilities.errorResponse("The phone is already in use");
        }

        Optional<User> userToUpdate = repository.findById(id);
        if (userToUpdate.isEmpty()) {
            log.info("User not found");
            return utilities.errorResponse("User not found");
        }

        userToUpdate.get().setEmail(user.getEmail());
        userToUpdate.get().setPhone(user.getPhone());
        userToUpdate.get().setName(user.getName());
        userToUpdate.get().setEnabled(user.getEnabled());
        userToUpdate.get().setUpdatedByIp(utilities.getClientIp());

        try {
            repository.save(userToUpdate.get());
            return utilities.successResponse("User updated successfully", null);
        }catch (Exception e){
            log.error("Error updating user: {}", e.getMessage());
            return utilities.exceptionResponse("Error updating user", e);
        }
    }

    @Override
    public GeneralResponse delete(Integer id) {
        log.info("Deleting user with id: {}", id);
        Optional<User> userToDelete = repository.findByPasiveIsFalseAndId(id);
        if (userToDelete.isEmpty()) {
            log.info("User not found");
            return utilities.errorResponse("User not found");
        }

        try {
            userToDelete.get().setPasive(true);
            userToDelete.get().setUpdatedByIp(utilities.getClientIp());
            repository.save(userToDelete.get());
            log.info("User deleted successfully");
            return utilities.successResponse("User deleted successfully", null);
        }catch (Exception e){
            log.error("Error deleting user: {}", e.getMessage());
            return utilities.exceptionResponse("Error deleting user", e);
        }
    }
}
