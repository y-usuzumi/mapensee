type Nullable<T> = T | null | undefined

export function isNull(obj: any): boolean {
    return obj === undefined || obj === null
}
