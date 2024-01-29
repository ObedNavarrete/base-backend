package com.security.api.service;

import com.security.api.dto.UserRecords;
import com.security.api.util.GlobalRecords;

public interface UserService {

    GlobalRecords.ApiResponse save(UserRecords.UserObject user, String role);

    GlobalRecords.ApiResponse findAll();

    GlobalRecords.ApiResponse update(UserRecords.UserObject user, Integer id);

    GlobalRecords.ApiResponse delete(Integer id);

    GlobalRecords.ApiResponse getByEmail(String email);

    GlobalRecords.ApiResponse getByPhone(String phone);

    GlobalRecords.ApiResponse getById(Integer id);

    GlobalRecords.ApiResponse addOrRemoveRoleToUser(Boolean add, UserRecords.UserRoleObject form);
}
