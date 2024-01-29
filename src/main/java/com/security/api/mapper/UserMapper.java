package com.security.api.mapper;

import com.security.api.dto.UserDto;
import com.security.api.model.entity.User;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface UserMapper {
    UserDto.UserLimited toUserLimited(User user);
    List<UserDto.UserLimited> toUserLimited(List<User> users);
    User toUser(UserDto.User user);
}
