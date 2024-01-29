package com.security.api.audit;

import com.security.api.model.entity.User;
import com.security.api.model.repository.UserRepository;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.AuditorAware;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class SpringSecurityAuditorAware implements AuditorAware<User> {
    private final UserRepository repo;

    @Override
    public Optional<User> getCurrentAuditor() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !authentication.isAuthenticated()) {
            return Optional.empty();
        }

        String id = authentication.getName();
        if (!isNumeric(id)) {
            return Optional.empty();
        }

        // to integer
        int userId = Integer.parseInt(id);
        User currentUser = repo.findById(userId).orElseThrow();
        return Optional.of(currentUser);
    }

    private boolean isNumeric(String str) {
        try {
            Integer.parseInt(str);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }
}