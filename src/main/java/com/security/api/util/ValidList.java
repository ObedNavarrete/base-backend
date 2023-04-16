package com.security.api.util;

import javax.validation.Valid;
import lombok.Data;
import lombok.experimental.Delegate;

import java.util.ArrayList;
import java.util.List;

/**
 * @comment: Esta clase es para poder validar una lista de objetos con @Valid, entonces,
 * en lugar de recibir en el controlador un List<ObjetoDTO>, se recibe un ValidList<ObjetoDTO>.
 * Con esto, se puede validar la lista completa de objetos, con las restricciones definidas en el DTO.
 */
@Data
public class ValidList<E> implements List<E> {
    @Valid
    @Delegate
    private List<E> list = new ArrayList<>();
}
