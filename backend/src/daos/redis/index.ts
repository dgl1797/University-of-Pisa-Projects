import Redis from "ioredis";

const redisdao = (
  port = parseInt(process?.env?.REDIS_PORT) ?? 6379,
  host = process?.env?.REDIS_HOST ?? "localhost",
  options?: Redis.RedisOptions
) => new Redis(port, host, options);

export default redisdao;

export class RedisBase {
  ["constructor"]: typeof RedisBase;
  static get prefix() {
    return "";
  }
  async set(key: string) {
    await redisdao().hmset(`${this.constructor.prefix}:${key}`, this as any);
    return `${this.constructor.prefix}:${key}`;
  }
  async setExpire(key: string, ttl: number) {
    await redisdao().hmset(`${this.constructor.prefix}:${key}`, this as any);
    await redisdao().expire(`${this.constructor.prefix}:${key}`, ttl);
    return `${this.constructor.prefix}:${key}`;
  }
  async get(key: string) {
    return await redisdao().hgetall(`${this.constructor.prefix}:${key}`);
  }
  async delete(key: string) {
    await redisdao().del(`${this.constructor.prefix}:${key}`);
    return `${this.constructor.prefix}:${key}`;
  }
  async exists(key: string) {
    return await redisdao().exists(`${this.constructor.prefix}:${key}`);
  }
}
