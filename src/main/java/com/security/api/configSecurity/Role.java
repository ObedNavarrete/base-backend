package com.security.api.configSecurity;

import jakarta.persistence.*;
import lombok.*;

@Getter @Setter
@Builder
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "role")
@SequenceGenerator(name = "role_id_seq", sequenceName = "role_id_seq", allocationSize = 1)
public class Role {
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "rol_id_seq")
    private Integer id;
    private String name;
    private String description;
}
