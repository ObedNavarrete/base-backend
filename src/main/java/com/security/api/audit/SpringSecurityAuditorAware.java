package com.security.api.audit;

import com.security.api.auth.base.User;
import com.security.api.auth.base.UserRepository;
import javax.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.AuditorAware;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
public class SpringSecurityAuditorAware implements AuditorAware<User> {
    @Autowired
    UserRepository repo;
    @Autowired
    private HttpServletRequest request;

    @Override
    public Optional<User> getCurrentAuditor() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !authentication.isAuthenticated()) {
            return Optional.empty();
        }

        String id = authentication.getName();
        // to integer
        int userId = Integer.parseInt(id);
        User currentUser = repo.findById(userId).orElseThrow();
        return Optional.of(currentUser);
    }
}
