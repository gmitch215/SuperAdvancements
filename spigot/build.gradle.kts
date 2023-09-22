import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

dependencies {
    api(project(":superadvancements"))
    api(project(":superadvancements-abstract"))

    listOf(
        "1_12_R1",
        "1_13_R1",
        "1_13_R2",
        "1_14_R1",
        "1_15_R1",
        "1_16_R1",
        "1_16_R2",
        "1_16_R3",
        "1_17_R1",
        "1_18_R1",
        "1_18_R2",
        "1_19_R1",
        "1_19_R2",
        "1_19_R3",
        "1_20_R1",
        "1_20_R2"
    ).forEach {
        api(project(":superadvancements-$it"))
    }

    compileOnly("org.spigotmc:spigot-api:1.12.2-R0.1-SNAPSHOT") {
        version {
            strictly("1.12.2-R0.1-SNAPSHOT")
        }
    }
}

description = "Bukkit & Spigot (String / BaseComponent) Implementation of the SuperAdvancements API"

java {
    withJavadocJar()
}

tasks {
    javadoc {
        sourceSets["main"].allJava.srcDir("src/main/javadoc")

        exclude("**/abstract/**", "**/nms/**")
    }

    register("sourcesJar", Jar::class.java) {
        archiveClassifier.set("sources")

        val sources = listOf(
            sourceSets["main"].allSource,
            project(":superadvancements").sourceSets["main"].allSource
        )

        from(sources)
    }

    withType<ShadowJar> {
        dependsOn("sourcesJar", "javadocJar")
    }
}

publishing {
    publications {
        getByName<MavenPublication>("maven") {
            artifact(tasks["sourcesJar"])
        }
    }
}