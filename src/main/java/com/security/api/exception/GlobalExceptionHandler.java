package com.security.api.exception;

import com.security.api.util.GlobalRecords;
import com.security.api.util.Utilities;
import io.jsonwebtoken.ExpiredJwtException;
import jakarta.validation.ConstraintViolationException;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import java.util.Locale;
import java.util.Objects;

import static com.security.api.util.ErrorCodes.DATA_INTEGRITY_VIOLATION;
import static com.security.api.util.ErrorCodes.INVALID_ARGUMENT;

@Slf4j
@RestControllerAdvice
@RequiredArgsConstructor
@Order(Ordered.HIGHEST_PRECEDENCE)
public class GlobalExceptionHandler extends ResponseEntityExceptionHandler {
    private final Utilities utilities;

    @ExceptionHandler(NullPointerException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(NullPointerException e) {
        var res = this.utilities.response(false, e.getMessage(), "NullPointerException has occurred");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(IllegalArgumentException e) {
        var res = this.utilities.response(false, e.getMessage(), "IlegalArgumentException has occurred");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(BadCredentialsException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(BadCredentialsException e) {
        var res = this.utilities.response(false, e.getMessage(), "Bad credentials");
        return ResponseEntity
                .status(HttpStatus.UNAUTHORIZED)
                .body(res);
    }

    @ExceptionHandler(IllegalStateException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(IllegalStateException e) {
        var res = this.utilities.response(false, e.getMessage(), "Illegal state exception");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(UnsupportedOperationException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(UnsupportedOperationException e) {
        var res = this.utilities.response(false, e.getMessage(), "Unsupported operation exception");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(NoSuchFieldException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(NoSuchFieldException e) {
        var res = this.utilities.response(false, e.getMessage(), "No such field exception");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(NoSuchMethodException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(NoSuchMethodException e) {
        var res = this.utilities.response(false, e.getMessage(), "No such method exception");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(NoSuchFieldError.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(NoSuchFieldError e) {
        var res = this.utilities.response(false, e.getMessage(), "No such field error");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(NoSuchMethodError.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(NoSuchMethodError e) {
        var res = this.utilities.response(false, e.getMessage(), "No such method error");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(NoClassDefFoundError.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(NoClassDefFoundError e) {
        var res = this.utilities.response(false, e.getMessage(), "No class definition found error");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(ClassNotFoundException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(ClassNotFoundException e) {
        var res = this.utilities.response(false, e.getMessage(), "Class not found exception");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(ClassCastException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(ClassCastException e) {
        var res = this.utilities.response(false, e.getMessage(), "Class cast exception");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(ArithmeticException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(ArithmeticException e) {
        var res = this.utilities.response(false, e.getMessage(), "Arithmetic exception");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(ArrayIndexOutOfBoundsException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(ArrayIndexOutOfBoundsException e) {
        var res = this.utilities.response(false, e.getMessage(), "Array index out of bounds exception");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(ArrayStoreException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(InterruptedException e) {
        var res = this.utilities.response(false, e.getMessage(), "Array store exception");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(InstantiationException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(InstantiationException e) {
        var res = this.utilities.response(false, e.getMessage(), "Instantiation exception");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(InternalError.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleException(InternalError e) {
        var res = this.utilities.response(false, e.getMessage(), "Internal error");
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(res);
    }

    @ExceptionHandler(AccessDeniedException.class)
    public final ResponseEntity<GlobalRecords.ApiResponse> handleAccessDeniedException(Exception ex) {
        var res = this.utilities.response(false, ex.getMessage(), "Access denied, you don't have permission to access this resource");
        return ResponseEntity
                .status(HttpStatus.FORBIDDEN)
                .body(res);
    }

    @ExceptionHandler(UsernameNotFoundException.class)
    public final ResponseEntity<GlobalRecords.ApiResponse> handleUsernameNotFoundException(Exception ex) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(utilities.exceptionResponse("Username not found", ex));
    }

    @ExceptionHandler(ConstraintViolationException.class)
    public final ResponseEntity<GlobalRecords.ApiResponse> handleConstraintViolationException(Exception ex) {
        var errors = ((ConstraintViolationException) ex).getConstraintViolations().stream()
                .map(x -> new GlobalRecords.ApiSubError(x.getPropertyPath().toString(), x.getMessage()))
                .toList();

        var errorObject = new GlobalRecords.ErrorObject(INVALID_ARGUMENT, errors);
        return ResponseEntity
                .status(HttpStatus.BAD_REQUEST)
                .body(this.utilities.response(false, ex.getMessage(), errorObject));
    }

    @ExceptionHandler(DataIntegrityViolationException.class)
    public final ResponseEntity<GlobalRecords.ApiResponse> handleDataIntegrityViolationException(Exception ex) {
        String message = ex.getCause().getCause().getMessage();
        message = message.substring(message.indexOf("Detail:"));
        log.error("DataIntegrityViolationException: " + message);
        return ResponseEntity
                .status(HttpStatus.BAD_REQUEST)
                .body(utilities.response(false, ex.getMessage(), new  GlobalRecords.ErrorObject(
                        DATA_INTEGRITY_VIOLATION,
                        message
                )));
    }

    @ExceptionHandler(value = {ExpiredJwtException.class})
    public ResponseEntity<GlobalRecords.ApiResponse> handleExpiredJwtException(ExpiredJwtException ex) {
        return ResponseEntity.status(401).body(utilities.exceptionResponse("Token has expired", ex));
    }

    @Override
    protected ResponseEntity<Object> handleMethodArgumentNotValid(
            MethodArgumentNotValidException ex,
            @NonNull HttpHeaders headers,
            @NonNull HttpStatusCode status,
            @NonNull WebRequest request) {
        log.error("MethodArgumentNotValidException: " + ex.getMessage());
        var errors = ex.getBindingResult().getFieldErrors().stream()
                .map(x -> new GlobalRecords.ApiSubError(x.getField(), Objects.requireNonNull(x.getDefaultMessage()).toLowerCase(Locale.ROOT)))
                .toList();

        var errorObject = new GlobalRecords.ErrorObject(INVALID_ARGUMENT, errors);
        return ResponseEntity
                .status(HttpStatus.BAD_REQUEST)
                .body(this.utilities.response(false, ex.getMessage(), errorObject));
    }

    @ExceptionHandler(CustomException.class)
    public ResponseEntity<GlobalRecords.ApiResponse> handleCustomException(CustomException e) {
        log.error("CustomException: " + e.getMessage());
        return ResponseEntity
                .status(e.getStatus())
                .body(utilities.response(false, e.getMessage(), e.getErrorObject()));
    }
}
