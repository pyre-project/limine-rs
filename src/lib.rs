#![no_std]
#![feature(const_nonnull_new)]

#[cfg(feature = "requests-section")]
pub use limine_proc::*;

mod file;
pub use file::*;

use core::{
    cell::UnsafeCell,
    ffi::{c_char, CStr},
    ptr::NonNull,
};

pub type Ptr<T> = Option<NonNull<T>>;

/// # Safety
///
/// Use this type *only* when the bootloader protocol guarantees it will be filled
/// with a pointer to valid, nul-terminated UTF-8 string data.
#[repr(transparent)]
struct NulStr(NonNull<c_char>);

impl<'a> From<&'a NulStr> for &'a str {
    fn from(value: &NulStr) -> Self {
        // Safety: Bootloader promises to provide a valid, nul-terminated ASCII string.
        unsafe { CStr::from_ptr(value.0.as_ptr()).to_str().unwrap_unchecked() }
    }
}

impl core::fmt::Debug for NulStr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(<&str>::from(self), f)
    }
}

/// Used to create the  request struct.
macro_rules! make_struct {
    (
        $(#[$meta:meta])*
        struct $name:ident: [$id1:expr, $id2:expr] => $response_ty:ty {
            $($(#[$field_meta:meta])* $field_name:ident : $field_ty:ty = $field_default:expr),*
        };
    ) => {
        $(#[$meta])*
        #[repr(C)]
        #[derive(Debug)]
        pub struct $name {
            id: [u64; 4],
            revision: u64,

            // XXX: The response is required to be wrapped inside an unsafe cell, since
            //      by default the response is set to NULL and when the compiler does not see
            //      any writes to the field, it is free to assume that the response is NULL. In
            //      our situation the bootloader mutates the field and we need to ensure that
            //      the compiler does not optimize the read away.
            response: UnsafeCell<Ptr<$response_ty>>,
            $($field_name: $field_ty),*
        }

        // Safety: Internal pointer is thread-agnostic (located in main memory).
        unsafe impl Send for $name {}

        impl $name {
            // XXX: The request ID is composed of 4 64-bit wide unsigned integers but the first
            //      two remain constant. This is refered as `_COMMON_MAGIC` in the  protocol
            //      header.
            pub const ID: [u64; 4] = [0xc7b1dd30df4c8b88, 0x0a82e883a194f07b, $id1, $id2];

            pub const fn new(revision: u64) -> Self {
                Self {
                    id: Self::ID,
                    revision,

                    response: UnsafeCell::new(None),
                    $($field_name: $field_default),*
                }
            }

            pub fn get_response<'a>(&'a self) -> Option<&'a $response_ty> {
                unsafe {
                    self.response
                        .get()
                        .read()
                        .map(|response_ptr| response_ptr.as_ref())
                }
            }

            pub fn get_response_mut<'a>(&'a mut self) -> Option<&'a mut $response_ty> {
                unsafe {
                    self.response
                        .get()
                        .read()
                        .map(|mut response_ptr| response_ptr.as_mut())
                }
            }

            // Generate a composable setter for each field.
            $($(#[$field_meta])* pub const fn $field_name(mut self, value: $field_ty) -> Self {
                self.$field_name = value;
                self
            })*
        }
    };
}

macro_rules! response_revision_impl {
    ($response_ty:ty) => {
        impl $response_ty {
            #[inline]
            pub const fn revision(&self) -> u64 {
                self.revision
            }
        }
    };
}

/* BOOT INFO */

make_struct!(
    struct BootInfoRequest: [0xf55038d8e2a1202f, 0x279426fcf5f59740] => BootInfoResponse {};
);

/// Limine response for the [`BootInfoRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct BootInfoResponse {
    revision: u64,
    name: NulStr,
    version: NulStr,
}

response_revision_impl!(BootInfoResponse);

impl BootInfoResponse {
    pub fn name(&self) -> &str {
        (&self.name).into()
    }

    pub fn version(&self) -> &str {
        (&self.version).into()
    }
}

/* STACK SIZE */

make_struct!(
    struct StackSizeRequest: [0x224ef0460a8e8926, 0xe1cb0fc25f46ea3d] => StackSizeResponse {
        /// The requested stack size (also used for SMP processors).
        stack_size: u64 = 0
    };
);

/// Limine response for the [`StackSizeRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct StackSizeResponse {
    revision: u64,
}

response_revision_impl!(StackSizeResponse);

/* HIGHER-HALF DIRECT MAP */

make_struct!(
    struct HhdmRequest: [0x48dcf1cb8ad2b852, 0x63984e959a98244b] => HhdmResponse {};
);

/// Limine response for the [`HhdmRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct HhdmResponse {
    revision: u64,

    /// The virtual address offset of the beginning of the higher half direct map.
    offset: u64,
}

response_revision_impl!(HhdmResponse);

impl HhdmResponse {
    #[inline]
    pub const fn offset(&self) -> u64 {
        self.offset
    }
}

/* 5 LEVEL PAGING */

make_struct!(
    /// The presence of this request will prompt the bootloader to turn on x86_64 5-level paging. It will not be
    /// turned on if this request is not present. If the response pointer is unchanged, 5-level paging is engaged.
    struct Level5PagingRequest: [0x94469551da9b3192, 0xebe5e86db7382888] => Level5PagingResponse {};
);

/// Limine response for the [`Level5PagingRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct Level5PagingResponse {
    revision: u64,
}

response_revision_impl!(Level5PagingResponse);

/* SMP */

// smp request tag:
#[repr(C)]
#[derive(Debug)]
pub struct CpuInfo {
    processor_id: u32,
    lapic_id: u32,
    reserved: u64,
    /// An atomic write to this field causes the parked CPU to jump to the
    /// written address, on a 64KiB (or Stack Size Request size) stack. A pointer
    /// to the struct [`SmpInfo`] structure of the CPU is passed in RDI. Other
    /// than that, the CPU state will be the same as described for the bootstrap
    /// processor. This field is unused for the structure describing the bootstrap
    /// processor.
    goto_address: Option<extern "C" fn(info: &CpuInfo) -> !>,
    /// A free for use field.
    extra_argument: u64,
}

impl CpuInfo {
    /// ACPI Processor UID as specified by the MADT.
    #[inline]
    pub const fn processor_id(&self) -> u32 {
        self.processor_id
    }

    /// Local APIC ID of the processor as specified by the MADT.
    #[inline]
    pub const fn lapic_id(&self) -> u32 {
        self.lapic_id
    }

    /// Extra argument, free for use by software.
    #[inline]
    pub fn get_argument(&self) -> u64 {
        self.extra_argument
    }

    pub fn jump_to(&mut self, func: extern "C" fn(info: &CpuInfo) -> !, arg: u64) {
        self.extra_argument = arg;
        self.goto_address = Some(func);

        // CPU will now be executing at `func` address.
    }
}

make_struct!(
    /// The presence of this request will prompt the bootloader to bootstrap the
    /// secondary processors. This will not be done if this request is not present.
    struct SmpRequest: [0x95a67b819a1b857e, 0xa0b61b723b6a73e0] => SmpResponse {
        /// Bit 0: Enable X2APIC, if possible.
        flags: u32 = 0
    };
);

/// Limine response for the [`SmpRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct SmpResponse {
    revision: u64,
    /// Bit 0: X2APIC has been enabled.
    flags: u32,
    /// The Local APIC ID of the bootstrap processor.
    bsp_lapic_id: u32,
    /// How many CPUs are present. It includes the bootstrap processor.
    cpu_count: u64,
    /// Pointer to an array of `cpu_count` pointers to struct [`SmpInfo`]
    /// structures.
    cpus_ptr: NonNull<CpuInfo>,
}

response_revision_impl!(SmpRequest);

impl SmpResponse {
    /// Return's the SMP info array pointer as a mutable rust slice.
    pub fn cpus<'a>(&'a mut self) -> &'a mut [CpuInfo] {
        unsafe {
            core::slice::from_raw_parts_mut(
                self.cpus_ptr.as_ptr(),
                self.cpu_count.try_into().unwrap(),
            )
        }
    }
}

/* MEMORY MAP */

#[repr(u32)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MemoryMapEntryType {
    Usable = 0,
    Reserved = 1,
    AcpiReclaimable = 2,
    AcpiNvs = 3,
    BadMemory = 4,
    BootloaderReclaimable = 5,
    /// The kernel and modules loaded are not marked as usable memory. They are
    /// marked as Kernel/Modules. The entries are guaranteed to be sorted by base
    /// address, lowest to highest. Usable and bootloader reclaimable entries are
    /// guaranteed to be 4096 byte aligned for both base and length. Usable and
    /// bootloader reclaimable entries are guaranteed not to overlap with any
    /// other entry. To the contrary, all non-usable entries (including kernel/modules)
    /// are not guaranteed any alignment, nor is it guaranteed that they do not
    /// overlap other entries.
    KernelAndModules = 6,
    Framebuffer = 7,
}

#[repr(C)]
#[derive(Debug)]
pub struct MemmapEntry {
    base: u64,
    len: u64,
    typ: MemoryMapEntryType,
}

make_struct!(
    struct MemmapRequest: [0x67cf3d9d378a806f, 0xe304acdfc50c3c62] => MemmapResponse {};
);

/// Limine response for the [`MemmapRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct MemmapResponse {
    revision: u64,
    /// How many memory map entries are present.
    entry_count: u64,
    /// Pointer to an array of `entry_count` pointers to struct [`MemmapEntry`] structures.
    entries_ptr: NonNull<MemmapEntry>,
}

response_revision_impl!(MemmapResponse);

impl MemmapResponse {
    pub fn memmap<'a>(&'a mut self) -> &'a mut [MemmapEntry] {
        unsafe {
            core::slice::from_raw_parts_mut(
                self.entries_ptr.as_ptr(),
                self.entry_count.try_into().unwrap(),
            )
        }
    }
}

/* ENTRY POINT */

type EntryPoint = extern "C" fn() -> !;

make_struct!(
    struct EntryPointRequest: [0x13d86c035a1cd3e1, 0x2b0caa89d8f3026a] => EntryPointResponse {
        /// The requested entry point.
        entry: Option<EntryPoint> = None
    };
);

/// Limine response for the [`EntryPointRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct EntryPointResponse {
    revision: u64,
}

response_revision_impl!(EntryPointResponse);

/* FRAMEBUFFER */

make_struct!(
    struct FramebufferRequest: [0x9d5827dcd881dd75, 0xa3148604f6fab11b] => FramebufferResponse {};
);

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ColorMask {
    size: u8,
    shift: u8,
}

impl ColorMask {
    #[inline]
    pub const fn size(&self) -> u8 {
        self.size
    }

    #[inline]
    pub const fn shift(&self) -> u8 {
        self.shift
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct Framebuffer {
    address: NonNull<u8>,
    width: u64,
    height: u64,
    pitch: u64,
    bpp: u16,
    memory_model: u8,
    red_mask: ColorMask,
    green_mask: ColorMask,
    blue_mask: ColorMask,
    reserved: [u8; 7],
    edid_size: u64,
    edid_ptr: NonNull<u8>,
    // TODO support rev 1
}

impl Framebuffer {
    #[inline]
    pub const fn address(&self) -> NonNull<u8> {
        self.address
    }

    #[inline]
    pub const fn width(&self) -> u64 {
        self.width
    }

    #[inline]
    pub const fn height(&self) -> u64 {
        self.height
    }

    #[inline]
    pub const fn pitch(&self) -> u64 {
        self.pitch
    }

    #[inline]
    pub const fn bpp(&self) -> u16 {
        self.bpp
    }

    #[inline]
    pub const fn memory_model(&self) -> u8 {
        self.memory_model
    }

    #[inline]
    pub const fn red_mask(&self) -> ColorMask {
        self.red_mask
    }

    #[inline]
    pub const fn green_mask(&self) -> ColorMask {
        self.green_mask
    }

    #[inline]
    pub fn blue_mask(&self) -> ColorMask {
        self.blue_mask
    }

    #[inline]
    pub fn edid<'a>(&'a self) -> &'a [u8] {
        unsafe {
            core::slice::from_raw_parts(self.edid_ptr.as_ptr(), self.edid_size.try_into().unwrap())
        }
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct FramebufferResponse {
    revision: u64,
    framebuffer_count: u64,
    framebuffers_ptr: NonNull<Framebuffer>,
}

response_revision_impl!(FramebufferResponse);

impl FramebufferResponse {
    pub fn framebuffers<'a>(&'a self) -> &'a [Framebuffer] {
        // Safety: Bootloader guarantees the pointer to be valid for `T: Framebuffer` of count.
        unsafe {
            core::slice::from_raw_parts(
                self.framebuffers_ptr.as_ptr(),
                self.framebuffer_count.try_into().unwrap(),
            )
        }
    }
}

/* KERNEL FILE */

make_struct!(
    struct KernelFileRequest: [0xad97e90e83f1ed67, 0x31eb5d1c5ff23b69] => KernelFileResponse {};
);

/// Limine response for the [`KernelFileRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct KernelFileResponse {
    revision: u64,
    /// Pointer to the struct [`File`] structure for the kernel file.
    kernel_file: NonNull<File>,
}

response_revision_impl!(KernelFileResponse);

impl KernelFileResponse {
    #[inline]
    pub fn file(&self) -> &File {
        // Safety: Bootloader guarantees the pointer to be valid, so long as a response was generated.
        unsafe { self.kernel_file.as_ref() }
    }
}

/* MODULES */

make_struct!(
    struct ModuleRequest: [0x3e7e279702be32af, 0xca1c4f3bd1280cee] => ModuleResponse {};
);

/// Limine response for the [`ModuleRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct ModuleResponse {
    revision: u64,
    module_count: u64,
    modules_ptr: NonNull<File>,
}

response_revision_impl!(ModuleResponse);

impl ModuleResponse {
    pub fn modules<'a>(&'a self) -> &'a [File] {
        unsafe {
            core::slice::from_raw_parts(
                self.modules_ptr.as_ptr(),
                self.module_count.try_into().unwrap(),
            )
        }
    }
}

/* RSDP */

make_struct!(
    struct RsdpRequest: [0xc5e77b6b397e7b43, 0x27637845accdcf3c] => RsdpResponse {};
);

/// Limine response for the [`RsdpRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct RsdpResponse {
    revision: u64,
    /// Address of the RSDP table.
    address: Ptr<u8>,
}

response_revision_impl!(RsdpResponse);

impl RsdpResponse {
    #[inline]
    pub const fn address(&self) -> Ptr<u8> {
        self.address
    }
}

/* SMBIOS */

make_struct!(
    struct SmbiosRequest: [0x9e9046f11e095391, 0xaa4a520fefbde5ee] => SmbiosResponse {};
);

/// Limine response for the [`SmbiosRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct SmbiosResponse {
    revision: u64,
    /// Address of the 32-bit SMBIOS entry point. NULL if not present.
    entry_32: Ptr<u8>,
    /// Address of the 64-bit SMBIOS entry point. NULL if not present.
    entry_64: Ptr<u8>,
}

response_revision_impl!(SmbiosResponse);

impl SmbiosResponse {
    #[inline]
    pub const fn entry_32bit(&self) -> Ptr<u8> {
        self.entry_32
    }

    #[inline]
    pub const fn entry_64bit(&self) -> Ptr<u8> {
        self.entry_64
    }
}

/* EFI SYSTEM TABLE */

make_struct!(
    struct EfiSystemTableRequest: [0x5ceba5163eaaf6d6, 0x0a6981610cf65fcc] => EfiSystemTableResponse {};
);

/// Limine response for the [`EfiSystemTableRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct EfiSystemTableResponse {
    revision: u64,
    address: NonNull<u8>,
}

response_revision_impl!(EfiSystemTableResponse);

impl EfiSystemTableResponse {
    #[inline]
    pub const fn address(&self) -> NonNull<u8> {
        self.address
    }
}

/* BOOT TIME */

make_struct!(
    struct BootTimeRequest: [0x502746e184c088aa, 0xfbc5ec83e6327893] => BootTimeResponse {};
);

/// Limine response for the [`BootTimeRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct BootTimeResponse {
    revision: u64,
    boot_time: i64,
}

response_revision_impl!(BootTimeResponse);

impl BootTimeResponse {
    #[inline]
    pub const fn unix_time(&self) -> i64 {
        self.boot_time
    }
}

/* KERNEL ADDRESS */

make_struct!(
    struct KernelAddressRequest: [0x71ba76863cc55f63, 0xb2644a48c516a487] => KernelAddressResponse {};
);

/// Limine response for the [`KernelAddressRequest`] protocol object.
#[repr(C)]
#[derive(Debug)]
pub struct KernelAddressResponse {
    revision: u64,
    /// The physical base address of the kernel.
    physical_base: u64,
    /// The virtual base address of the kernel.
    virtual_base: u64,
}

response_revision_impl!(KernelAddressResponse);

impl KernelAddressResponse {
    #[inline]
    pub const fn physical_base(&self) -> u64 {
        self.physical_base
    }

    #[inline]
    pub const fn virtual_base(&self) -> u64 {
        self.virtual_base
    }
}

/* DEVICE TREE BLOB */

make_struct!(
    struct DtbRequest: [0xb40ddb48fb54bac7, 0x545081493f81ffb7] => DtbResponse {};
);

/// Limine response for the [`DtbRequest`] protocol object.
///
/// ## Note
///
/// Information contained in the `/chosen` node may not reflect the information
/// given by bootloader tags, and as such the `/chosen` node properties should
/// be ignored.
#[repr(C)]
#[derive(Debug)]
pub struct DtbResponse {
    revision: u64,
    /// Virtual pointer to the device tree blob.
    dtb_ptr: NonNull<u8>,
}

response_revision_impl!(DtbResponse);

impl DtbResponse {
    #[inline]
    pub const fn ptr(&self) -> NonNull<u8> {
        self.dtb_ptr
    }
}
