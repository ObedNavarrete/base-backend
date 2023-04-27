package com.security.api.configuration;

import com.security.api.auth.base.UserRepository;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.jsonwebtoken.ExpiredJwtException;
import lombok.RequiredArgsConstructor;
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

    @Override
    protected void doFilterInternal(
            @NonNull HttpServletRequest request,
            @NonNull HttpServletResponse response,
            @NonNull FilterChain filterChain) throws ServletException, IOException {
        final String authorizationHeader = request.getHeader("Authorization");
        final String jwt;
        final String idString;
        final String applicationJson = "application/json";

        if (authorizationHeader == null || !authorizationHeader.startsWith("Bearer ")) {
            filterChain.doFilter(request, response);
            return;
        }

        jwt = authorizationHeader.substring(7);

        boolean tokenExpired;
        tokenExpired = jwtService.isTokenExpired(jwt);
        if (tokenExpired){
            response.setContentType(applicationJson);
            response.setStatus(403);
            response.getWriter().write("{ \"token_expired\": \"The token has expired\" }");;
            response.getWriter().flush();
            response.getWriter().close();
            return;
        }

        idString = jwtService.extractUsername(jwt);

        /**
         *  @comment: jwtService.hasClaim(jwt) is true if contains "roles" claim, false otherwise
         *  in case of not containing "roles" claim, then this is a refresh token
         */
        if (!jwtService.hasClaim(jwt)) {
            response.setContentType(applicationJson);
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            response.getWriter().write("{ \"message\": \"Invalid token\" }");
            response.getWriter().flush();
            response.getWriter().close();
            return;
        }

        if (idString != null && SecurityContextHolder.getContext().getAuthentication() == null) {

            int idInt = Integer.parseInt(idString);
            UserDetails userDetails = this.repository.findByPasiveIsFalseAndId(idInt)
                    .orElse(null);

            if (userDetails == null) {
                response.setContentType(applicationJson);
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                response.getWriter().write("{ \"message\": \"El status del usuario ha cambiado\" }");
                response.getWriter().flush();
                response.getWriter().close();
                return;
            }

            /**
             * @comment: check if user.getRoles() is empty,
             * in case of empty, then user is not authorized
             */
            if (userDetails.getAuthorities().isEmpty()){
                response.setContentType(applicationJson);
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                response.getWriter().write("{ \"error_roles\": \"User has no roles\" }");
                response.getWriter().flush();
                response.getWriter().close();
                return;
            }

            /**
             * @comment: check if user is enabled,
             * is enabled in case of property "enabled" is true and "pasive" is false
             */
            if (Boolean.FALSE.equals((userDetails.isEnabled()))) {
                response.setContentType(applicationJson);
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
