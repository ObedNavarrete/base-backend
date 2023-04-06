package com.security.api.service;

import com.security.api.dto.UserDTO;
import com.security.api.util.GeneralResponse;

public interface UserService {
    GeneralResponse save(UserDTO user);

    GeneralResponse findAll();
}
