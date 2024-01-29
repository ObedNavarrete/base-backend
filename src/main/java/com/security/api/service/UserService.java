package com.security.api.service;

import com.security.api.dto.UserDto;
import com.security.api.util.GlobalRecords;

public interface UserService {

    GlobalRecords.ApiResponse save(UserDto.User user, String role);

    GlobalRecords.ApiResponse findAll();

    GlobalRecords.ApiResponse update(UserDto.User user, Integer id);

    GlobalRecords.ApiResponse delete(Integer id);

    GlobalRecords.ApiResponse getByEmail(String email);

    GlobalRecords.ApiResponse getByPhone(String phone);

    GlobalRecords.ApiResponse getById(Integer id);

    GlobalRecords.ApiResponse addOrRemoveRoleToUser(Boolean add, UserDto.UserRole form);
}
