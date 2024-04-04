package com.security.api.configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.security.api.model.repository.UserRepository;
import com.security.api.service.JwtService;
import com.security.api.util.Utilities;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import lombok.RequiredArgsConstructor;
import org.springframework.lang.NonNull;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.Objects;

import static com.security.api.util.ErrorCodes.JWT_ERROR;
import static org.springframework.util.MimeTypeUtils.APPLICATION_JSON;

@Component
@RequiredArgsConstructor
public class JwtAuthFilter extends OncePerRequestFilter {
    private final ObjectMapper mapper = new ObjectMapper();
    private final JwtService jwtService;
    private final UserRepository repository;
    private final Utilities util;

    @Override
    protected void doFilterInternal(
            @NonNull HttpServletRequest request,
            @NonNull HttpServletResponse response,
            @NonNull FilterChain filterChain) throws ServletException, IOException {
        final String authorizationHeader = request.getHeader("Authorization");
        final String jwt;
        final String idString;

        if (authorizationHeader == null || !authorizationHeader.startsWith("Bearer ")) {
            filterChain.doFilter(request, response);
            return;
        }

        jwt = authorizationHeader.substring(7);
        boolean tokenExpired;
        tokenExpired = jwtService.isTokenExpired(jwt);
        if (tokenExpired) {
            var r = this.util.responseForFilter(
                    "Token expired",
                    HttpServletResponse.SC_UNAUTHORIZED,
                    JWT_ERROR,
                    "The token has expired at " + jwtService.extractExpiration(jwt)
            );

            response.setContentType(String.valueOf(APPLICATION_JSON));
            response.setStatus(403);
            response.getWriter().write(mapper.writeValueAsString(r));
            response.getWriter().flush();
            response.getWriter().close();
            return;
        }

        idString = jwtService.extractUsername(jwt);
        if (!jwtService.hasClaim(jwt)) {
            var r = this.util.responseForFilter(
                    "Invalid token",
                    HttpServletResponse.SC_UNAUTHORIZED,
                    JWT_ERROR,
                    "Invalid token, this is a refresh token"
            );

            response.setContentType(String.valueOf(APPLICATION_JSON));
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            response.getWriter().write(mapper.writeValueAsString(r));
            response.getWriter().flush();
            response.getWriter().close();
            return;
        }

        if (idString != null && SecurityContextHolder.getContext().getAuthentication() == null) {
            int idInt = Integer.parseInt(idString);
            UserDetails userDetails = this.repository.findByPassiveIsFalseAndId(idInt)
                    .orElse(null);

            if (Objects.isNull(userDetails)) {
                var r = this.util.responseForFilter(
                        "User not found",
                        HttpServletResponse.SC_UNAUTHORIZED,
                        JWT_ERROR,
                        "The status of the user has changed"
                );

                response.setContentType(String.valueOf(APPLICATION_JSON));
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                response.getWriter().write(mapper.writeValueAsString(r));
                response.getWriter().flush();
                response.getWriter().close();
                return;
            }

            if (userDetails.getAuthorities().isEmpty()) {
                var r = this.util.responseForFilter(
                        "User has no roles",
                        HttpServletResponse.SC_UNAUTHORIZED,
                        JWT_ERROR,
                        "User has no roles"
                );

                response.setContentType(String.valueOf(APPLICATION_JSON));
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                response.getWriter().write(mapper.writeValueAsString(r));
                response.getWriter().flush();
                response.getWriter().close();
                return;
            }

            if (Boolean.FALSE.equals((userDetails.isEnabled()))) {
                var r = this.util.responseForFilter(
                        "User is disabled",
                        HttpServletResponse.SC_UNAUTHORIZED,
                        JWT_ERROR,
                        "User is disabled"
                );

                response.setContentType(String.valueOf(APPLICATION_JSON));
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                response.getWriter().write(mapper.writeValueAsString(r));
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
                var r = this.util.responseForFilter(
                        "Invalid token",
                        HttpServletResponse.SC_UNAUTHORIZED,
                        JWT_ERROR,
                        "The token has expired"
                );

                response.setContentType(String.valueOf(APPLICATION_JSON));
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                response.getWriter().write(mapper.writeValueAsString(r));
                response.getWriter().flush();
                response.getWriter().close();
                return;
            }

            filterChain.doFilter(request, response);
        }
    }
}
