package com.security.api;

import com.security.api.auth.base.User;
import com.security.api.audit.SpringSecurityAuditorAware;
import javax.annotation.PostConstruct;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.data.domain.AuditorAware;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

import java.util.TimeZone;

@EnableJpaAuditing
@SpringBootApplication
public class SecurityApiApplication {

    public static void main(String[] args) {
        SpringApplication.run(SecurityApiApplication.class, args);
        System.out.println("The application is running...");
    }

    @Bean
    @Primary
    public AuditorAware<User> auditorProvider() {
        return new SpringSecurityAuditorAware();
    }

    @PostConstruct
    void started() {
        TimeZone.setDefault(TimeZone.getTimeZone("America/Managua"));
    }
}
