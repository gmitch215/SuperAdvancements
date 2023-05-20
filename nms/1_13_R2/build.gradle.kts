val mcVersion = "1.13.2"

dependencies {
    api(project(":superadvancements-abstract"))

    compileOnly("org.spigotmc:spigot:$mcVersion-R0.1-SNAPSHOT")
    testImplementation("org.spigotmc:spigot:$mcVersion-R0.1-SNAPSHOT")
}