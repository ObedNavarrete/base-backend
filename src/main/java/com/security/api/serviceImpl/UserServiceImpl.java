package com.security.api.serviceImpl;

import com.security.api.configSecurity.Role;
import com.security.api.configSecurity.RoleRepository;
import com.security.api.configSecurity.User;
import com.security.api.configSecurity.UserRepository;
import com.security.api.dto.UserDTO;
import com.security.api.mapper.UserMapper;
import com.security.api.service.UserService;
import com.security.api.util.GeneralResponse;
import com.security.api.util.Utilities;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Slf4j
@RequiredArgsConstructor
public class UserServiceImpl extends Utilities implements UserService {
    private final UserRepository repository;
    private final UserMapper mapper;
    private final RoleRepository roleRepository;
    private final PasswordEncoder passwordEncoder;

    @Override
    public GeneralResponse save(UserDTO user){
        log.info("Saving user");
        if (Boolean.TRUE.equals(repository.existsByEmail(user.getEmail()))){
            return this.errorResponse("The email is already in use");
        }
        if (Boolean.TRUE.equals(repository.existsByPhone(user.getPhone()))){
            return this.errorResponse("The phone is already in use");
        }

        User userToSave = mapper.toEntity(user);

        Set<Role> roles = user.getRoles().stream().map(role -> roleRepository.findByName(role.getName()).orElse(null)).collect(Collectors.toSet());

        userToSave.setRoles(roles);
        userToSave.setPasive(false);
        userToSave.setCreatedByIp(this.getClientIp());
        userToSave.setPassword(passwordEncoder.encode(user.getPassword()));

        try {
            repository.save(userToSave);
            return this.successResponse("User saved successfully", null);
        }catch (Exception e){
            log.error("Error saving user: {}", e.getMessage());
            return this.exceptionResponse("Error saving user", e);
        }
    }

    @Override
    public GeneralResponse findAll(){
        log.info("Getting all users");
        try {
            List<User> users = repository.findAll();
            if (users.isEmpty())
                return this.errorResponse("No users found"
            );

            return this.successResponse("Users found successfully", mapper.toDTO(users));
        }catch (Exception e){
            log.error("Error getting users: {}", e.getMessage());
            return this.exceptionResponse("Error getting users", e);
        }
    }
}
