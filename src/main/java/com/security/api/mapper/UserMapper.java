package com.security.api.mapper;

import com.security.api.dto.UserRecords;
import com.security.api.model.entity.User;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface UserMapper {
    UserRecords.UserLimitedObject toUserLimitedRecord(User user);
    List<UserRecords.UserLimitedObject> toUserLimitedRecords(List<User> users);
    User toUser(UserRecords.UserObject userObject);
}
