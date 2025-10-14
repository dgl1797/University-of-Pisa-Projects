function throwError(code: number, message: string): never {
  throw { code, message };
}

export { throwError };
