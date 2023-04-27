package com.security.api.exception;

import com.security.api.util.GeneralResponse;
import com.security.api.util.Utilities;
import io.jsonwebtoken.ExpiredJwtException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import java.util.Locale;
import java.util.Objects;
import java.util.stream.Collectors;

@RestControllerAdvice
@RequiredArgsConstructor
@Slf4j
public class GlobalExceptionHandler extends ResponseEntityExceptionHandler {
    private final Utilities utilities = new Utilities();

    @ExceptionHandler(NullPointerException.class)
    public ResponseEntity<GeneralResponse> handleException(NullPointerException e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("Null pointer exception")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<GeneralResponse> handleException(IllegalArgumentException e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("Illegal argument exception")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(BadCredentialsException.class)
    public ResponseEntity<GeneralResponse> handleException(BadCredentialsException e) {
        String comment = e.getMessage();
        comment = Objects.requireNonNullElse(comment, "Your credentials are incorrect");
        String message = "error";

        return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                .body(GeneralResponse.builder()
                        .status("401")
                        .message(message)
                        .data(null)
                        .comment(comment)
                        .build());
    }

    @ExceptionHandler(IllegalStateException.class)
    public ResponseEntity<GeneralResponse> handleException(IllegalStateException e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("Illegal state exception")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(UnsupportedOperationException.class)
    public ResponseEntity<GeneralResponse> handleException(UnsupportedOperationException e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("Unsupported operation exception")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(NoSuchFieldException.class)
    public ResponseEntity<GeneralResponse> handleException(NoSuchFieldException e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("No such field exception")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(NoSuchMethodException.class)
    public ResponseEntity<GeneralResponse> handleException(NoSuchMethodException e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("No such method exception")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(NoSuchFieldError.class)
    public ResponseEntity<GeneralResponse> handleException(NoSuchFieldError e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("No such field error")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(NoSuchMethodError.class)
    public ResponseEntity<GeneralResponse> handleException(NoSuchMethodError e) {
        // No such method error to exception
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("No such method error")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(NoClassDefFoundError.class)
    public ResponseEntity<GeneralResponse> handleException(NoClassDefFoundError e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("No class definition found error")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(ClassNotFoundException.class)
    public ResponseEntity<GeneralResponse> handleException(ClassNotFoundException e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("Class not found exception")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(ClassCastException.class)
    public ResponseEntity<GeneralResponse> handleException(ClassCastException e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("Class cast exception")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(ArithmeticException.class)
    public ResponseEntity<GeneralResponse> handleException(ArithmeticException e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("Arithmetic exception")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(ArrayIndexOutOfBoundsException.class)
    public ResponseEntity<GeneralResponse> handleException(ArrayIndexOutOfBoundsException e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("Array index out of bounds exception")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(ArrayStoreException.class)
    public ResponseEntity<GeneralResponse> handleException(InterruptedException e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("Array store exception")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(InstantiationException.class)
    public ResponseEntity<GeneralResponse> handleException(InstantiationException e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("Instantiation exception")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(InternalError.class)
    public ResponseEntity<GeneralResponse> handleException(InternalError e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(GeneralResponse.builder()
                        .status("500")
                        .message("Internal error")
                        .data(null)
                        .comment("Internal server error")
                        .build());
    }

    @ExceptionHandler(AccessDeniedException.class)
    public final ResponseEntity<GeneralResponse> handleAccessDeniedException(Exception ex, WebRequest request) {
        return ResponseEntity.status(401).body(
                GeneralResponse.builder()
                        .status("401")
                        .message("You don't have permission to access this resource")
                        .data(null)
                        .comment("Access denied")
                        .build()
        );
    }

    @ExceptionHandler(UsernameNotFoundException.class)
    public final ResponseEntity<GeneralResponse> handleUsernameNotFoundException(Exception ex, WebRequest request) {
        return ResponseEntity.status(403).body(utilities.exceptionResponse("Username not found", ex));
    }

    @ExceptionHandler(DataIntegrityViolationException.class)
    public final ResponseEntity<GeneralResponse> handleDataIntegrityViolationException(Exception ex, WebRequest request) {
        log.error("DataIntegrityViolationException: " + ex.getMessage());
        String message = ex.getCause().getCause().getMessage();
        message = message.substring(message.indexOf("Detail:"));
        log.error("DataIntegrityViolationException: " + message);
        return ResponseEntity.status(400).body(utilities.errorResponse(message));
    }

    @ExceptionHandler(value = {ExpiredJwtException.class})
    public ResponseEntity<GeneralResponse> handleExpiredJwtException(ExpiredJwtException ex) {
        return ResponseEntity.status(401).body(utilities.exceptionResponse("Token expired", ex));
    }

    @Override
    protected ResponseEntity<Object> handleMethodArgumentNotValid(
            MethodArgumentNotValidException ex, HttpHeaders headers,
            HttpStatus status, WebRequest request) {

        String comentario = ex.getBindingResult().getFieldErrors().stream()
                .map(x -> "FATAL ERROR: Column '" + x.getField() + "' => " + x.getDefaultMessage().toLowerCase(Locale.ROOT))
                .collect(Collectors.joining(", "));

        return ResponseEntity.ok(utilities.errorResponse(comentario));
    }

    @Override
    public ResponseEntity<Object> handleMissingServletRequestParameter(MissingServletRequestParameterException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        return ResponseEntity.ok(utilities.errorResponse(ex.getMessage()));
    }

    @Override
    protected ResponseEntity<Object> handleHttpRequestMethodNotSupported(HttpRequestMethodNotSupportedException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        return ResponseEntity.ok(utilities.errorResponse(ex.getMessage()));
    }
}
