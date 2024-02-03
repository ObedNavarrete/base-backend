package com.security.api.util;

import com.security.api.exception.CustomException;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import static com.security.api.util.ErrorCodes.INVALID_ARGUMENT;
import static com.security.api.util.ErrorCodes.METHOD_OBJECT_VALIDATION;

@Slf4j
@Component
@RequiredArgsConstructor
public class ObjectValidator {
    private final ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
    private final Validator validator = factory.getValidator();

    public <T> void validate(T objectToValidate) {
        var violations = validator.validate(objectToValidate);
        if (!violations.isEmpty()) {
            log.error("Validation failed for object: {}", objectToValidate);
            violations.forEach(v -> log.error("Field: {}, Message: {}", v.getPropertyPath(), v.getMessage()));
            var subErrors = violations.stream()
                    .map(v -> new GlobalRecords.ApiSubError(
                            v.getPropertyPath().toString(),
                            v.getMessage()
                    ))
                    .toList();
            var errorObject = new GlobalRecords.ErrorObject(INVALID_ARGUMENT, subErrors);
            throw new CustomException(METHOD_OBJECT_VALIDATION, 400, errorObject);
        }
    }
}
