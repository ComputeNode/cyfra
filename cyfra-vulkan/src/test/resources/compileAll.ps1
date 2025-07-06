Get-ChildItem -Filter *.comp -Name | ForEach-Object -Process {
    $name = $_.Replace(".comp", "")
    "$Env:VULKAN_SDK\Bin\glslangValidator.exe -V $name.comp -o $name.spv" | Invoke-Expression
}
