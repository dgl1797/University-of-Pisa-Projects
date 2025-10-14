import { env } from "$env/dynamic/private";

export function load({ url }) {
  const queryString = decodeURIComponent(url.searchParams?.get("query") ?? "");
  return { host: env.PYTHON_APIS, queryString };
}
