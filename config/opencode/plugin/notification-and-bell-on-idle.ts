import Bun from "bun";
import type { Plugin } from "@opencode-ai/plugin";

export const NotificationPlugin: Plugin = async () => ({
  event: async ({ event }) => {
    if (event.type === "session.idle" || event.type === "session.compacted") {
      await ding(event.type);
    }
  },
  "permission.ask": async (input) => {
    await ding(input.title);
  },
});

function ding(message: string) {
  return Promise.all([
    Bun.$`notify-send -t 5000 'opencode' '${message}'`,
    Bun.write(Bun.stdout, "\x07"),
  ]);
}
