package com.security.api.service;

import com.security.api.util.GlobalRecords;

public interface AuthService {
    GlobalRecords.ApiResponse register(GlobalRecords.RegisterRequest request);

    GlobalRecords.ApiResponse login(GlobalRecords.LoginRequest request);

    GlobalRecords.ApiResponse refresh(String token);
}
