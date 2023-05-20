package me.gamercoder215.superadvancements.v1_14_R1;

import com.mojang.bridge.game.GameVersion;

import java.util.Date;
import java.util.UUID;

final class MockWorldVersion1_14_R1 implements GameVersion {

    private final String id = UUID.nameUUIDFromBytes("test".getBytes()).toString().replace("-", "");
    private final Date build = new Date();

    public MockWorldVersion1_14_R1() {}

    @Override
    public Date getBuildTime() {
        return build;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public String getName() {
        return "test";
    }

    @Override
    public String getReleaseTarget() {
        return "1.16.2";
    }

    @Override
    public int getWorldVersion() {
        return 2578;
    }

    @Override
    public int getPackVersion() {
        return 6;
    }

    @Override
    public int getProtocolVersion() {
        return 751;
    }

    @Override
    public boolean isStable() {
        return false;
    }

}
