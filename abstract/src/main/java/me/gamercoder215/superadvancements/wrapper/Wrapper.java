package me.gamercoder215.superadvancements.wrapper;

import org.bukkit.Bukkit;

public interface Wrapper {

    Wrapper w = getWrapper();

    String PACKET_INJECTOR_ID = "superadvancements:packet_injector";

    // Static Util

    static String getServerVersion() {
        return Bukkit.getServer().getClass().getPackage().getName().split("\\.")[3].substring(1);
    }

    static Wrapper getWrapper() {
        String v = getServerVersion();
        try {
            return Class.forName("me.gamercoder215.superadvancements.v" + v + ".Wrapper" + v)
                .asSubclass(Wrapper.class)
                .getConstructor()
                .newInstance();
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("Unknown Wrapper Version: " + v, e);
        }
    }

}
