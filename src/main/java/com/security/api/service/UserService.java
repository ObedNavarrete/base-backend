package com.security.api.service;

import com.security.api.dto.UserDTO;
import com.security.api.dto.UserRoleDTO;
import com.security.api.util.GeneralResponse;

public interface UserService {

    GeneralResponse save(UserDTO user, String role);

    GeneralResponse findAll();

    GeneralResponse update(UserDTO user, Integer id);

    GeneralResponse delete(Integer id);

    GeneralResponse getByEmail(String email);

    GeneralResponse getByPhone(String phone);

    GeneralResponse getById(Integer id);

    GeneralResponse addOrRemoveRoleToUser(Boolean add, UserRoleDTO form);
}
