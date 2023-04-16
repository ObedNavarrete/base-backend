package com.security.api.configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.security.api.auth.base.UserRepository;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.security.api.util.ErrorResponse;
import io.jsonwebtoken.ExpiredJwtException;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.lang.NonNull;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
@RequiredArgsConstructor
public class JwtAuthenticationFilter extends OncePerRequestFilter {
    private final JwtService jwtService;
    private final UserRepository repository;

    private void writeErrorResponse(HttpServletResponse response, int status, String message) throws IOException {
        response.setStatus(status);
        response.setContentType("application/json");
        response.getWriter().write(new ObjectMapper().writeValueAsString(new ErrorResponse(status, HttpStatus.valueOf(status).getReasonPhrase(), message)));
    }

    @Override
    protected void doFilterInternal(
            @NonNull HttpServletRequest request,
            @NonNull HttpServletResponse response,
            @NonNull FilterChain filterChain) throws ServletException, IOException {
        final String authorizationHeader = request.getHeader("Authorization");
        final String jwt;
        final String userEmailOrPhone;

        if (authorizationHeader == null || !authorizationHeader.startsWith("Bearer ")) {
            filterChain.doFilter(request, response);
            return;
        }

        jwt = authorizationHeader.substring(7);

        boolean tokenExpired;
        tokenExpired = jwtService.isTokenExpired(jwt);
        if (tokenExpired){
            response.setContentType("application/json");
            response.setStatus(403);
            response.getWriter().write("{ \"token_expired\": \"The token has expired\" }");;
            response.getWriter().flush();
            response.getWriter().close();
            return;
        }

        userEmailOrPhone = jwtService.extractUsername(jwt);

        /**
         *  @comment: jwtService.hasClaim(jwt) is true if contains "roles" claim, false otherwise
         *  in case of not containing "roles" claim, then this is a refresh token
         */
        if (!jwtService.hasClaim(jwt)) {
            response.setContentType("application/json");
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            response.getWriter().write("{ \"message\": \"Invalid token\" }");
            response.getWriter().flush();
            response.getWriter().close();
            return;
        }

        if (userEmailOrPhone != null && SecurityContextHolder.getContext().getAuthentication() == null) {
            UserDetails userDetails = this.repository.findById(Integer.parseInt(userEmailOrPhone))
                    .orElseThrow(() -> new RuntimeException("User not found"));

            /**
             * @comment: check if user is enabled,
             * is enabled in case of property "enabled" is true and "pasive" is false
             */
            if (Boolean.FALSE.equals((userDetails.isEnabled()))) {
                response.setContentType("application/json");
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                response.getWriter().write("{ \"message\": \"User is disabled\" }");
                response.getWriter().flush();
                response.getWriter().close();
                return;
            }

            if (Boolean.TRUE.equals(jwtService.isTokenValid(jwt, userDetails))) {
                UsernamePasswordAuthenticationToken authToken =
                        new UsernamePasswordAuthenticationToken(
                                userDetails,
                                null,
                                userDetails.getAuthorities()
                        );
                authToken.setDetails(
                        new WebAuthenticationDetailsSource().buildDetails(request)
                );

                SecurityContextHolder.getContext().setAuthentication(authToken);
            } else {
                throw new ExpiredJwtException(null, null, "Token expired");
            }

            filterChain.doFilter(request, response);
        }
    }
}
