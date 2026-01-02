export const hash_map_from = (xs) => {
    const result = {};
    for (let i = 0; i < xs.length; i += 2) {
        result[xs[i]] = xs[i + 1];
    }
    return result;
};

export const debug_assert = (a, b) => {
    return JSON.stringify(a) === JSON.stringify(b);
}

export const re_find = (p, i) => {
    const result = p.exec(i);
    if (result == null) {
        return null;
    }
    return result[0];
}

export const swap_BANG_ = (atom, f) => {
    const result = atom[0];
    atom[0] = f(result);
    return result;
}

export const _PLUS_ = (...xs) => {
    return xs.reduce((a, b) => a + b);
}

export const _MINUS_ = (a, b) => {
    return a - b
}

export const inc = (a) => {
    return a + 1;
}

export const update = (m, k, f) => {
    return { ...m, [k]: f(m[k]) };
}