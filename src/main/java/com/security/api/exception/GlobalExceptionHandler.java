package com.security.api.exception;

import com.security.api.util.GeneralResponse;
import com.security.api.util.Utilities;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.Nullable;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import java.util.Locale;
import java.util.stream.Collectors;

@RestControllerAdvice
@RequiredArgsConstructor
@Slf4j
public class GlobalExceptionHandler extends ResponseEntityExceptionHandler {
    private final Utilities utilities = new Utilities();
    @ExceptionHandler(Exception.class)
    public GeneralResponse handleException(Exception e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(NullPointerException.class)
    public GeneralResponse handleException(NullPointerException e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(IllegalArgumentException.class)
    public GeneralResponse handleException(IllegalArgumentException e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(IllegalStateException.class)
    public GeneralResponse handleException(IllegalStateException e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(UnsupportedOperationException.class)
    public GeneralResponse handleException(UnsupportedOperationException e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(NoSuchFieldException.class)
    public GeneralResponse handleException(NoSuchFieldException e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(NoSuchMethodException.class)
    public GeneralResponse handleException(NoSuchMethodException e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(NoSuchFieldError.class)
    public GeneralResponse handleException(NoSuchFieldError e) {
        return utilities.errorResponse("No such field error " + e.getMessage());
    }

    @ExceptionHandler(NoSuchMethodError.class)
    public GeneralResponse handleException(NoSuchMethodError e) {
        // No such method error to exception
        return utilities.errorResponse("No such method error " + e.getMessage());
    }

    @ExceptionHandler(NoClassDefFoundError.class)
    public GeneralResponse handleException(NoClassDefFoundError e) {
        return utilities.errorResponse("No class definition found error " + e.getMessage());
    }

    @ExceptionHandler(ClassNotFoundException.class)
    public GeneralResponse handleException(ClassNotFoundException e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(ClassCastException.class)
    public GeneralResponse handleException(ClassCastException e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(ArithmeticException.class)
    public GeneralResponse handleException(ArithmeticException e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(ArrayIndexOutOfBoundsException.class)
    public GeneralResponse handleException(ArrayIndexOutOfBoundsException e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(ArrayStoreException.class)
    public GeneralResponse handleException(ArrayStoreException e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(InterruptedException.class)
    public GeneralResponse handleException(InterruptedException e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(InstantiationException.class)
    public GeneralResponse handleException(InstantiationException e) {
        return utilities.exceptionResponse("Error", e);
    }

    @ExceptionHandler(InternalError.class)
    public GeneralResponse handleException(InternalError e) {
        return utilities.errorResponse("Internal error");
    }

    @ExceptionHandler(AccessDeniedException.class)
    public final ResponseEntity<GeneralResponse> handleAccessDeniedException(Exception ex, WebRequest request) {
        return ResponseEntity.status(403).body(utilities.exceptionResponse("Unauthorized", ex));
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

    @Override
    protected ResponseEntity<Object> handleExceptionInternal(
            Exception ex,
            @Nullable Object body,
            HttpHeaders headers,
            HttpStatusCode statusCode,
            WebRequest request) {
        // Personalize handleHttpRequestMethodNotSupported
        return ResponseEntity.badRequest().body(utilities.errorResponse(ex.getMessage()));
        /*String comentario = ex.getBindingResult().getFieldErrors().stream()
                .map(x -> "FATAL ERROR: Column '" + x.getField() + "' => " + x.getDefaultMessage().toLowerCase(Locale.ROOT))
                .collect(Collectors.joining(", "));

        return ResponseEntity.ok(utilities.errorResponse(comentario));*/
    }

    /*@Override
    protected ResponseEntity<Object> handleMissingServletRequestParameter(
            MissingServletRequestParameterException ex,
            HttpHeaders headers,
            HttpStatus status,
            WebRequest request) {
        return ResponseEntity.ok(utilities.errorResponse(ex.getMessage()));
    }

    @Override
    protected ResponseEntity<Object> handleHttpRequestMethodNotSupported(
            HttpRequestMethodNotSupportedException ex,
            HttpHeaders headers,
            HttpStatus status,
            WebRequest request) {
        String comment ="El método " + ex.getMethod() + " no es sportado ... use el método " + ex.getSupportedHttpMethods();
        return ResponseEntity.ok(utilities.errorResponse(comment));
    }*/
}
